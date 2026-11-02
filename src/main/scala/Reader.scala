package com.alecdorrington.codeswitch

import cats.effect.{Deferred, IO, Ref}
import cats.effect.std.Queue
import cats.effect.syntax.all.*
import cats.syntax.all.*
import com.alecdorrington.megalingual.Language
import com.alecdorrington.omnibrowser.Detection
import fs2.Stream
import org.scalajs.dom.{console, Node}

/**
  * What the reader asks of the rest of the extension: to `translate` words, for
  * the `hints` in passages in a language being learnt, each entry of either's
  * reply given, by its index, to what is given with the request as soon as it
  * arrives, before the rest; to `render` passages in a foreign language in the
  * reader's own, each in parts; to `detect` the languages of a text; and how
  * well the reader knows each word that they have rated (see [[Familiarity]]).
  */
final case class Asking
  (
    translate: Translate,
    hints: (Passages, Arrivals.Each[List[Hint]]) => IO[List[List[Hint]]],
    render: List[List[String]] => IO[List[List[String]]],
    detect: String => IO[Detection],
    familiarity: (Language, List[String]) => IO[Map[String, Double]],
  )

/**
  * Translates words, giving each translation, by the word's index, to what is
  * given with them as soon as it arrives, before the rest.
  */
type Translate =
  (List[Word], Arrivals.Each[Translation]) => IO[List[Translation]]

/**
  * Translates a share of the words of the page that the reader is reading, in
  * its passages in the languages they read fluently, marks the words above the
  * reader's level in its passages in the languages being learnt, and translates
  * its passages in any other language whole, if the reader would have them.
  */
object Reader:

  /**
    * How many words in sight to ask for at once. A model writes its reply a
    * word at a time, so a small batch is answered sooner.
    */
  private val nearBatch = 10

  /** How many words out of sight to ask for at once. */
  private val batch = 40

  /**
    * About how many words of passages in sight to ask for hints in at once, and
    * so of each excerpt of the passages out of sight.
    */
  private val nearExcerpts = 100

  /**
    * About how many words of each excerpt of the passages in sight, whose hints
    * are shown as soon as the model has written them, before the rest of the
    * reply, so that the first are soon shown.
    */
  private val sightExcerpts = 40

  /** About how many words of passages out of sight to ask for hints in at once. */
  private val excerpts = 300

  /**
    * About how many characters of passages in sight to translate whole at once,
    * by characters rather than words, as a language without spaces has few.
    */
  private val nearCharacters = 600

  /**
    * About how many characters of passages out of sight to translate whole at
    * once, and the most of each request, as its translation must fit in one
    * reply.
    */
  private val characters = 2400

  /** How many requests to make at once, for translations and for hints each. */
  private val parallel = 6

  /**
    * How many passages' languages to find at once, which the browser does on
    * the device.
    */
  private val detections = 16

  /**
    * Marks words of the page, and of whatever is added to it, for each learner
    * that `learners` gives in turn, putting the original words back when it
    * changes, translating at each intensity that `intensities` chooses in turn.
    *
    * @param learners
    *   The reader, then each change of their choices, or `None` while nothing
    *   is to be marked.
    * @param intensities
    *   The share of the words to translate, then each new choice.
    * @param asking
    *   What the reader asks of the rest of the extension.
    * @param tips
    *   The tooltips of the translations and hints.
    */
  def read
    (
      learners: Stream[IO, Option[Learner]],
      intensities: Stream[IO, Double],
      asking: Asking,
      tips: Tips,
    )
    : IO[Unit] = (Ref[IO].of(0.0), Deferred[IO, Unit]).flatMapN(
    (intensity, known) =>
      follow(intensities, intensity, known)
        .both(
          learners
            .zipWithIndex
            .switchMap((learner, index) =>
              Stream.exec(IO(Highlighter.restore(tips))) ++
                Stream
                  .emits(learner.toList)
                  .flatMap(learner =>
                    Stream.exec(known.get) ++ pass(
                      learner,
                      intensity,
                      asking,
                      tips,
                      index.toInt,
                    ),
                  ),
            )
            .compile
            .drain,
        )
        .void,
  )

  /**
    * Keeps each intensity that `intensities` chooses in `intensity`, completing
    * `known` with the first, and shows the translations whose draws are below
    * it, hiding the rest, at once.
    */
  private def follow
    (
      intensities: Stream[IO, Double],
      intensity: Ref[IO, Double],
      known: Deferred[IO, Unit],
    )
    : IO[Unit] = intensities
    .map(Words.bounded)
    .evalMap(current =>
      intensity.set(current) *> known.complete(()) *>
        IO(Highlighter.reveal(current)),
    )
    .compile
    .drain

  /**
    * Marks words of the page in `pass`, for `learner`, at `intensity`, then of
    * what is added to it, and of the page again each time that `intensity`
    * rises, until a request fails. Each time, it first forgets what it marked
    * in what the page has since taken out.
    */
  private def pass
    (
      learner: Learner,
      intensity: Ref[IO, Double],
      asking: Asking,
      tips: Tips,
      pass: Int,
    )
    : Stream[IO, Unit] = Stream
    .eval(Ref[IO].of(Option.empty[Double]))
    .flatMap(last =>
      Highlighter
        .nodes(rose(learner, intensity, last))
        .evalMap(roots =>
          IO(Highlighter.prune(tips)) *> detect(roots, asking.detect, pass) *>
            read(roots, learner, asking, tips, pass)
              .both(translateAll(
                roots,
                learner,
                intensity,
                asking.translate,
                tips,
                pass,
              ))
              .both(render(roots, learner, asking.render, tips, pass))
              .void,
        ),
    )
    .handleErrorWith(error =>
      Stream.exec(IO(console.error(s"Codeswitch: ${ error.getMessage }"))),
    )

  /**
    * Whether the page is to be marked whole: first at once, even in a hidden
    * tab, as a page has always been translated as it loads, if anything would
    * come of it for `learner` (see [[Learner.busy]]), and then each time that
    * `intensity` rises above `last`, as it was when last asked. A rise while
    * the page is hidden waits until it is shown, so that the tabs in the
    * background do not all ask for translations at once.
    */
  private def rose
    (
      learner: Learner,
      intensity: Ref[IO, Double],
      last: Ref[IO, Option[Double]],
    )
    : IO[Boolean] = (intensity.get, IO(Highlighter.outOfSight)).flatMapN(
    (current, hidden) =>
      last.modify:
        case None                     => (Some(current), learner.busy(current))
        case Some(previous) if hidden => (Some(previous.min(current)), false)
        case Some(previous)           => (Some(current), current > previous),
  )

  /**
    * Finds the language of each passage beneath `roots` not yet known in
    * `pass`, several at once: as the browser finds it, or, where it cannot tell
    * reliably, or finds one not known, as the page says it is, or else as most
    * of the page's words were found to be in the pass.
    */
  private def detect
    (
      roots: List[Node],
      detect: String => IO[Detection],
      pass: Int,
    )
    : IO[Unit] = IO(Highlighter.unknown(roots, pass)).flatMap(
    _.parTraverseN(detections)((block, text) =>
      detect(text).flatMap(detection =>
        IO(Highlighter.learn(
          block,
          pass,
          foundLanguage(
            detection.language,
            Highlighter.declared(block),
          ),
          text,
        )),
      ),
    ) *> IO(Highlighter.prevail(roots, pass)),
  )

  /**
    * Shows the hints in each passage beneath `roots` in a language that
    * `learner` is learning, not yet read in `pass`, those in sight first, each
    * as the reader's familiarity with its word says, if they have rated it,
    * covered by `tips`. Each request is of passages in one language.
    */
  private def read
    (
      roots: List[Node],
      learner: Learner,
      asking: Asking,
      tips: Tips,
      pass: Int,
    )
    : IO[Unit] = IO(
    Words
      .nearestFirst(
        Highlighter.unread(
          roots,
          pass,
          learner,
          block =>
            if Highlighter.distanceOf(block) == 0 then sightExcerpts
            else nearExcerpts,
        ),
        nearExcerpts,
        excerpts,
      )(
        _.words,
        excerpt => Highlighter.distance(excerpt.range),
      )
      .flatMap(apart(_)(_.learnt)),
  ).flatMap(_.parTraverseN(parallel)(passages =>
      Ref[IO]
        .of(Set.empty[Int])
        .flatMap(shown =>
          arriving[List[Hint], List[List[Hint]]](asking.hints(
            Passages(
              passages.head.learnt,
              passages.map(_.text),
            ),
            _,
          ))((index, hints) =>
            marking(passages, shown, asking, tips)(List(index -> hints)),
          ).flatMap(answered =>
            IO.fromEither(answered)
              .flatMap(found =>
                marking(passages, shown, asking, tips)(
                  found.zipWithIndex.map(_.swap),
                ),
              ),
          ),
        ),
    ))
    .void

  /**
    * What `request` gives, or why it failed, doing `each` with each entry of
    * its reply that arrives before the rest, in turn, in this fiber rather than
    * in the one that hears of it, so that none is shown once this one is
    * cancelled, as when the page is restored for another pass.
    */
  private def arriving[A, B]
    (request: Arrivals.Each[A] => IO[B])
    (each: Arrivals.Each[A])
    : IO[Either[Throwable, B]] = Queue
    .unbounded[IO, Option[(Int, A)]]
    .flatMap(queue =>
      def drain: IO[Unit] = queue
        .take
        .flatMap(_.fold(IO.unit)((index, entry) => each(index, entry) *> drain))
      request((index, entry) => queue.offer(Some(index -> entry)))
        .guarantee(queue.offer(None))
        .attempt
        .both(drain)
        .map(_._1),
    )

  /**
    * Translates whole each passage beneath `roots` in a foreign language that
    * `learner` has translated, not yet rendered in `pass`, those in sight
    * first, showing each part's translation in its place, the original covered
    * by `tips`.
    */
  private def render
    (
      roots: List[Node],
      learner: Learner,
      render: List[List[String]] => IO[List[List[String]]],
      tips: Tips,
      pass: Int,
    )
    : IO[Unit] = IO(
    Words.nearestFirst(
      Highlighter.foreign(roots, pass, learner, characters),
      nearCharacters,
      characters,
    )(
      _.size,
      passage => Highlighter.distance(passage.range),
    ),
  ).flatMap(_.parTraverseN(parallel)(passages =>
      render(passages.map(_.parts.map(_.text))).flatMap(translated =>
        IO(
          passages
            .zip(translated)
            .foreach((passage, parts) =>
              Highlighter.render(passage, parts, tips),
            ),
        ),
      ),
    ))
    .void

  /**
    * Shows each of `found`, the hints of one of `excerpts` by its index, unless
    * it is in `shown`, which then holds it, each as the reader's familiarity
    * with its word says, covered by `tips`.
    */
  private def marking
    (
      excerpts: List[Excerpt],
      shown: Ref[IO, Set[Int]],
      asking: Asking,
      tips: Tips,
    )
    (found: List[(Int, List[Hint])])
    : IO[Unit] = unshown(shown, excerpts, found).flatMap(fresh =>
    asking
      .familiarity(
        excerpts.head.learnt.language,
        fresh.flatMap(_._2).map(_.word),
      )
      .flatMap(known =>
        IO(fresh.foreach((excerpt, hints) =>
          Highlighter.hint(
            excerpt,
            hints.map(Familiarity(known, _)),
            tips,
          ),
        )),
      ),
  )

  /**
    * Each of `entries` with the one of `items` at its index, but those whose
    * indices are in `shown`, which then holds them all, so that an entry that
    * arrives early is not shown again with the rest.
    */
  private def unshown[A, B]
    (
      shown: Ref[IO, Set[Int]],
      items: List[A],
      entries: List[(Int, B)],
    )
    : IO[List[(A, B)]] = shown.modify(before =>
    (
      before ++ entries.map(_._1),
      entries.collect {
        case (index, entry)
          if items.isDefinedAt(index) && !before.contains(index) =>
          items(index) -> entry
      },
    ),
  )

  /**
    * `items` split into those of each `key`, in the order in which the keys
    * first come, each keeping its items' order.
    */
  private def apart[A, K](items: List[A])(key: A => K): List[List[A]] = items
    .map(key)
    .distinct
    .map(each => items.filter(key(_) == each))

  /**
    * Translates the words chosen beneath `roots` at the intensity, those in
    * sight first, and shows their translations, covered by `tips`.
    */
  private def translateAll
    (
      roots: List[Node],
      learner: Learner,
      intensity: Ref[IO, Double],
      translate: Translate,
      tips: Tips,
      pass: Int,
    )
    : IO[Unit] = intensity
    .get
    .flatMap(current =>
      IO(
        Words.nearestFirst(
          Highlighter.choose(roots, pass, current, learner),
          nearBatch,
          batch,
        )(
          _ => 1,
          choice => Highlighter.distance(choice.range),
        ),
      ),
    )
    .flatMap(_.parTraverseN(parallel)(
      translateBatch(_, intensity, translate, tips),
    ))
    .void

  /**
    * Translates `choices`, which are in the order of the page, showing the
    * translations of each passage together as soon as the model has moved on to
    * the next passage, and the rest, and those that did not arrive, once it has
    * answered, or failed, covered by `tips`. A passage's are shown together so
    * that of two that overlap, the later is shown (see [[Highlighter.show]]),
    * as a noun is with its article.
    */
  private def translateBatch
    (
      choices: List[Choice],
      intensity: Ref[IO, Double],
      translate: Translate,
      tips: Tips,
    )
    : IO[Unit] = (Ref[IO].of(Set.empty[Int]), Ref[IO].of(Held.none)).flatMapN(
    (shown, held) =>
      arriving[Translation, List[Translation]](
        translate(choices.map(_.word), _),
      )((index, translation) =>
        unshown(
          shown,
          choices,
          List(index -> translation),
        ).flatMap(_.traverse_((choice, translation) =>
          IO(Highlighter.passage(choice)).flatMap(_.traverse_(passage =>
            held
              .modify(_.adding(passage, choice -> translation))
              .flatMap(showing(intensity, tips)),
          )),
        )),
      ).flatMap(answered =>
        (
          held.getAndSet(Held.none).map(_.words),
          unshown(
            shown,
            choices,
            answered.toOption.toList.flatMap(_.zipWithIndex.map(_.swap)),
          ),
        ).flatMapN((before, rest) =>
          showing(intensity, tips)(
            (before ++ rest).sortBy((choice, _) => choices.indexOf(choice)),
          ),
        ) *> IO.fromEither(answered).void,
      ),
  )

  /**
    * The translations that have arrived of the words of `passage`, the last
    * that any arrived in, not yet shown, in the order of the page.
    */
  private final case class Held
    (
      passage: Option[Node],
      words: List[(Choice, Translation)],
    ):

    /**
      * These with `word` of `passage`, and those to show at once: these, if
      * `passage` is another.
      */
    def adding
      (
        passage: Node,
        word: (Choice, Translation),
      )
      : (Held, List[(Choice, Translation)]) =
      if this.passage.forall(_ == passage) then
        (Held(Some(passage), words :+ word), Nil)
      else (Held(Some(passage), List(word)), words)

  private object Held:

    /** None. */
    val none: Held = Held(None, Nil)

  /**
    * Shows each of `words`' translations, if its draw is below the intensity as
    * it is by then, and hides it otherwise, again if the intensity changed
    * meanwhile, covered by `tips`.
    */
  private def showing
    (intensity: Ref[IO, Double], tips: Tips)
    (words: List[(Choice, Translation)])
    : IO[Unit] = intensity
    .get
    .flatMap(before =>
      IO(Highlighter.show(
        words.map(_._1),
        words.map(_._2),
        before,
        tips,
      )) *>
        intensity
          .get
          .flatMap(after =>
            IO(Highlighter.reveal(after)).whenA(after != before),
          ),
    )
    .whenA(words.nonEmpty)
