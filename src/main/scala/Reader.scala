package com.alecdorrington.codeswitch

import cats.effect.{Deferred, IO, Ref}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import com.alecdorrington.megalingual.Language
import com.alecdorrington.omnibrowser.Detection
import fs2.Stream
import org.scalajs.dom.{console, Node}

/**
  * What the reader asks of the rest of the extension: to `translate` words, for
  * the `hints` in passages in the language being learnt, to `detect` the
  * languages of a text, and how well the reader knows each word that they have
  * rated (see [[Familiarity]]).
  */
final case class Asking
  (
    translate: List[Word] => IO[List[Translation]],
    hints: List[String] => IO[List[List[Hint]]],
    detect: String => IO[Detection],
    familiarity: (Language, List[String]) => IO[Map[String, Double]],
  )

/**
  * Translates a share of the words of the page that the reader is reading, in
  * its passages in other languages, and marks the words above the reader's
  * level in its passages in the language being learnt.
  */
object Reader:

  /** How many words to ask for at once. */
  private val batch = 40

  /** About how many words of passages to ask for hints in at once. */
  private val excerpts = 300

  /** How many requests to make at once. */
  private val parallel = 4

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
              .whenA(learner.reads)
              .both(
                translateAll(
                  roots,
                  learner,
                  intensity,
                  asking.translate,
                  tips,
                  pass,
                ).whenA(learner.translating),
              )
              .void,
        ),
    )
    .handleErrorWith(error =>
      Stream.exec(IO(console.error(s"Codeswitch: ${ error.getMessage }"))),
    )

  /**
    * Whether the page is to be marked whole: first at once, even in a hidden
    * tab, as a page has always been translated as it loads, and then, if
    * `learner` is translating, each time that `intensity` rises above `last`,
    * as it was when last asked. A rise while the page is hidden waits until it
    * is shown, so that the tabs in the background do not all ask for
    * translations at once.
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
        case None =>
          (Some(current), learner.reads || learner.translating && current > 0)
        case Some(previous) if hidden => (Some(previous.min(current)), false)
        case Some(previous)           =>
          (Some(current), learner.translating && current > previous),
  )

  /**
    * Finds the language of each passage beneath `roots` not yet known in
    * `pass`, several at once: as the browser finds it, or, where it cannot tell
    * reliably, as the page says it is.
    */
  private def detect
    (
      roots: List[Node],
      detect: String => IO[Detection],
      pass: Int,
    )
    : IO[Unit] = IO(Highlighter.unknown(roots, pass)).flatMap(
    _.parTraverseN(detections)((block, text) =>
        detect(text).flatMap(found =>
          IO(Highlighter.learn(
            block,
            pass,
            found.language.fold(Highlighter.declared(block))(Language.fromTag),
          )),
        ),
      )
      .void,
  )

  /**
    * Shows the hints in each passage beneath `roots` in the language that
    * `learner` is learning, not yet read in `pass`, each as the reader's
    * familiarity with its word says, if they have rated it, covered by `tips`.
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
    Words.weighed(
      Highlighter.unread(roots, pass, learner.language, excerpts),
      excerpts,
    )(_.words),
  ).flatMap(_.parTraverseN(parallel)(passages =>
      asking
        .hints(passages.map(_.text))
        .flatMap(found =>
          asking
            .familiarity(
              learner.language,
              found.flatten.map(_.word),
            )
            .flatMap(known =>
              IO(
                passages
                  .zip(found)
                  .foreach((passage, hints) =>
                    Highlighter.hint(
                      passage,
                      hints.map(Familiarity(known, _)),
                      tips,
                    ),
                  ),
              ),
            ),
        ),
    ))
    .void

  private def translateAll
    (
      roots: List[Node],
      learner: Learner,
      intensity: Ref[IO, Double],
      translate: List[Word] => IO[List[Translation]],
      tips: Tips,
      pass: Int,
    )
    : IO[Unit] = intensity
    .get
    .flatMap(current =>
      IO(Words.batches(
        Highlighter.choose(roots, pass, current, learner.language),
        batch,
      )),
    )
    .flatMap(_.parTraverseN(parallel)(
      translateBatch(_, intensity, translate, tips),
    ))
    .void

  /**
    * Translates `choices`, then shows each translation whose draw is below the
    * intensity as it is by then, and hides the rest, again if the intensity
    * changed meanwhile, covered by `tips`.
    */
  private def translateBatch
    (
      choices: List[Choice],
      intensity: Ref[IO, Double],
      translate: List[Word] => IO[List[Translation]],
      tips: Tips,
    )
    : IO[Unit] = translate(choices.map(_.word)).flatMap(translations =>
    intensity
      .get
      .flatMap(before =>
        IO(Highlighter.show(choices, translations, before, tips)) *>
          intensity
            .get
            .flatMap(after =>
              IO(Highlighter.reveal(after)).whenA(after != before),
            ),
      ),
  )
