package com.alecdorrington.codeswitch.services

import cats.effect.{Deferred, IO, Ref, SyncIO}
import cats.effect.std.Semaphore
import cats.syntax.all.*
import com.alecdorrington.codeswitch.lexicon.{
  Derivation, Dictionary, Homographs, Lexeme, Record, Source, Spelling, Store,
  Timestamp,
}
import com.alecdorrington.codeswitch.models.{
  alike, whole, Entry, Lookup, Wiktionary,
}
import com.alecdorrington.iris.{Model, ModelClient}
import com.alecdorrington.megalingual.Language
import com.alecdorrington.omnibrowser.{Browser, StorageKey}
import org.scalajs.dom
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/**
  * Looks words up for the card in the reader's own lexicon, kept on this device
  * for good: as a dictionary the reader has not turned off gives them, fetched
  * the first time a word is looked up, its texts put into the reader's language
  * by a model, or else as a model generates them, where no dictionary has the
  * word. Only the background uses it, which keeps Iris out of the pages.
  */
object Lexicon:

  /**
    * Runs a program with the reader's model and a client of it, asked for only
    * when a model is needed, as it needs the reader's key.
    */
  type Ask = [A] => ((Model, ModelClient[IO]) => IO[A]) => IO[A]

  /**
    * The text at a URL, or none if there is none there, or a failure if it
    * cannot be had.
    */
  type Fetch = String => IO[Option[String]]

  /**
    * The dictionaries that the reader has turned off, of which nothing is
    * fetched or shown, each kept by its name: none at first.
    */
  val off = StorageKey.sync[Set[Dictionary]]("dictionariesOff", Set.empty)

  /** The name of the database in which the lexicon is kept. */
  private val database = "lexicon"

  private val opened = Ref.in[SyncIO, IO, Option[Store]](None).unsafeRunSync()

  /**
    * The lexicon, opened the first time it is asked for: the browser's, or one
    * in memory where there is none, as in a test.
    */
  def store: IO[Store] = opened
    .get
    .flatMap(
      _.fold(opening(database)(
        Store.browser(database).flatTap(store => opened.set(Some(store))),
      ))(IO.pure),
    )

  /** Uses `store` as the lexicon from now on, as a test does. */
  private[codeswitch] def using(store: Store): IO[Unit] =
    opened.set(Some(store))

  /**
    * The entry for `lookup`, a word of a language being learnt, explained in
    * `native`, the reader's own, as kept in `store`, of what the dictionaries
    * that the reader has not turned off in `storage` give: Wiktionary's, from
    * `fetch`, the first time the word is looked up, of the word's class, with
    * the gloss that best fits its meaning, its texts put into the reader's
    * language by a model if that is not English; or else, if no dictionary has
    * the word, one that a model generates.
    *
    * If a model cannot put Wiktionary's texts into the reader's language, as
    * when they have given no key, they are given in English, and none is kept,
    * so that they are put into it once it can. Each part of a lookup (fetching
    * a word, putting its texts into the reader's language, generating one)
    * asked for again while it is being found waits for it, rather than asking
    * again (see [[Once]]), and a model is not asked again for a minute after it
    * failed (see [[unlessFailed]]).
    */
  def entry
    (
      native: Language,
      lookup: Lookup,
      store: Store,
      storage: Browser.Storage,
      fetch: Fetch,
      ask: Ask,
    )
    : IO[Option[Entry]] = storage
    .get(off)
    .flatMap(off =>
      val shown =
        (source: Source) => Derivation.dictionaryOf(source).forall(!off(_))
      val learnt = lookup.language
      learn(learnt, lookup.word, store, fetch)
        .unlessA(off(Dictionary.Wiktionary))
        .flatMap(_ => found(learnt, lookup, native, store, shown))
        .flatMap {
          case Nil => generated(native, lookup, store, storage, ask, shown)
          case found if native.alike(Language.English) =>
            IO.pure(choose(found, learnt, native, lookup, shown))
          case found => translated(native, lookup, found, store, ask, shown)
              .map(choose(_, learnt, native, lookup, shown))
        },
    )

  /**
    * Learns `word`, of `learnt`, from Wiktionary, by `fetch`, unless its
    * spelling has been checked there already: each lexeme of Wiktionary's
    * entries for it, merged with what is kept of it, each of its spellings
    * listed among its homographs, and the record fetched, kept. That Wiktionary
    * has none of it is kept too, as asking again would find none again.
    */
  private[codeswitch] def learn
    (
      learnt: Language,
      word: String,
      store: Store,
      fetch: Fetch,
    )
    : IO[Unit] =
    val (spelling, url) = (Spelling(learnt, word), Wiktionary.url(learnt, word))
    store
      .homographs(List(spelling))
      .map(_.head.checked.contains(learnt.whole))
      .ifM(
        IO.unit,
        fetching(url)((fetch(url), Timestamp.now).flatMapN((lines, at) =>
          record(
            learnt,
            spelling,
            Source.Sourced(Dictionary.Wiktionary, url, at),
            lines,
            store,
          ).void,
        )),
      )

  /**
    * Keeps what `lines`, the record of `source` for the word spelt `spelling`
    * of `learnt`, if any, gives: each of its lexemes, merged with what is kept
    * of it, each spelling of those listed among its homographs, and that
    * `spelling` has been checked.
    */
  private def record
    (
      learnt: Language,
      spelling: Spelling,
      source: Source.Sourced,
      lines: Option[String],
      store: Store,
    )
    : IO[List[Lexeme]] = keeping(
    store,
    lines.fold(Nil)(Derivation.fromKaikki(learnt, _, source)),
    List(spelling),
    homographs =>
      if homographs.spelling == spelling then
        homographs.checkedIn(learnt.whole, source.at)
      else homographs,
    lines.map(Record(source, _)).toList,
  )

  /**
    * Keeps `found`, each merged with what is kept of it, each of its spellings
    * listed among its homographs, as are `spellings`, each as `change` changes
    * it, and `records`, one change to the lexicon at a time, lest two that
    * overlap lose one. It gives the lexemes as kept.
    */
  private def keeping
    (
      store: Store,
      found: List[Lexeme],
      spellings: List[Spelling],
      change: Homographs => Homographs,
      records: List[Record],
    )
    : IO[List[Lexeme]] = lock
    .permit
    .surround(
      for
        kept <- store.lexemes(found.map(_.id))
        merged = found.map(lexeme =>
          Derivation.merged(kept.find(_.id == lexeme.id), lexeme),
        )
        spelt = merged.flatMap(lexeme =>
          (lexeme.lemma :: lexeme.forms).map(_.spelling -> lexeme.id),
        )
        listed <- store.homographs((spellings ++ spelt.map(_._1)).distinct)
        homographs = listed.map(homographs =>
          change(homographs.including(spelt.collect {
            case (spelling, id) if spelling == homographs.spelling => id
          })),
        )
        _ <- store.keep(merged, homographs, records)
      yield merged,
    )

  /**
    * The lexemes kept of `lookup`'s word, spelt alike in `learnt`, that a card
    * may show a reader of `native`, of only what `shown` lets it show: each
    * with senses, of the word's class, if any are, as when a word is two words,
    * as `Bank`, a bench, and `Bank`, a bank (see [[Wiktionary.candidates]]),
    * and only those with a dictionary's senses, if any have, as a model
    * generates a word only where no dictionary has it.
    */
  private def found
    (
      learnt: Language,
      lookup: Lookup,
      native: Language,
      store: Store,
      shown: Source => Boolean,
    )
    : IO[List[Lexeme]] =
    val spelling = Spelling(learnt, lookup.word)
    store
      .homographs(List(spelling))
      .flatMap(homographs => store.lexemes(homographs.head.lexemes))
      .map(lexemes =>
        val alike = lexemes.filter(lexeme =>
          lexeme.lemma.spelling == spelling &&
          lexeme.languages.exists(_.whole == learnt.whole),
        )
        val sourced = alike.filter(Derivation.sourced(_, shown))
        val spelt   = if sourced.nonEmpty then sourced else alike
        Wiktionary
          .candidates(
            spelt.map(Derivation.entry(_, learnt, native, shown)),
            lookup,
          )
          .map((_, index) => spelt(index)),
      )

  /**
    * The entry of `found` that a card shows a reader of `native`, of the one
    * whose glosses best fit the meaning of `lookup`.
    */
  private def choose
    (
      found: List[Lexeme],
      learnt: Language,
      native: Language,
      lookup: Lookup,
      shown: Source => Boolean,
    )
    : Option[Entry] = Wiktionary.choose(
    found.map(Derivation.entry(_, learnt, native, shown)),
    lookup.meaning,
  )

  /**
    * `found`, the lexemes that may be `lookup`'s, with each text from a source
    * that a card would show a reader of `native` in English put into their
    * language by a model, all at once, then kept; or else as they are.
    */
  private def translated
    (
      native: Language,
      lookup: Lookup,
      found: List[Lexeme],
      store: Store,
      ask: Ask,
      shown: Source => Boolean,
    )
    : IO[List[Lexeme]] =
    val learnt  = lookup.language
    val missing = found.filter(Derivation.untranslated(_, native, shown))
    val name = ("explained" :: native.code :: missing.map(_.id.text)).mkString(
      ":",
    )
    if missing.isEmpty then IO.pure(found)
    else
      translating(name)(
        unlessFailed(name)(ask((model, client) =>
          noting(name)(
            Translator
              .explained(
                client,
                learnt,
                native,
                missing.map(Derivation.english(_, learnt, shown)),
              )
              .flatMap(IO.fromOption(_)(Problem(
                "Wiktionary's entries could not be put into your language.",
              ))),
          ).tupleLeft(model),
        )).flatMap((model, entries) =>
          Timestamp
            .now
            .flatMap(at =>
              lock
                .permit
                .surround(
                  store
                    .lexemes(missing.map(_.id))
                    .flatMap(kept =>
                      val translated = kept
                        .zip(entries)
                        .map((lexeme, entry) =>
                          Derivation.translated(
                            lexeme,
                            entry,
                            native,
                            model,
                            at,
                            shown,
                          ),
                        )
                      store.keep(translated, Nil, Nil).as(translated),
                    ),
                ),
            ),
        ),
      ).attempt
        .map(_.fold(
          _ => found,
          translated =>
            found.map(lexeme =>
              translated.find(_.id == lexeme.id).getOrElse(lexeme),
            ),
        ))

  /**
    * A model's entry for `lookup`, in `native`, the reader's own language, as
    * kept, or else generated, then kept, or none if it generated none, which is
    * kept too, as asking again would cost as much each time.
    */
  private def generated
    (
      native: Language,
      lookup: Lookup,
      store: Store,
      storage: Browser.Storage,
      ask: Ask,
      shown: Source => Boolean,
    )
    : IO[Option[Entry]] =
    val (key, learnt) = (none(native, lookup), lookup.language)
    storage
      .get(key)
      .ifM(
        IO.none,
        generating(key.name)(
          unlessFailed(key.name)(ask((model, client) =>
            noting(key.name)(Translator.defined(client, native, lookup))
              .tupleLeft(model),
          )).flatMap {
            case (_, None) =>
              storage.set(key, true).handleError(_ => ()).as(Nil)
            case (model, Some(entry)) => Timestamp
                .now
                .flatMap(at =>
                  keeping(
                    store,
                    List(
                      Derivation.generated(entry, learnt, native, model, at),
                    ),
                    Nil,
                    identity,
                    Nil,
                  ),
                )
          },
        ).map(
          _.headOption
            .map(Derivation.entry(_, learnt, native, shown))
            .filter(_.senses.nonEmpty),
        ),
      )

  /**
    * Whether a model generated no entry for `lookup` in `native`, the reader's
    * own language, when asked: the word last, so that none can be taken for
    * another's.
    */
  def none(native: Language, lookup: Lookup): StorageKey[Boolean] = StorageKey
    .local(
      List(
        "none",
        lookup.language.whole.code,
        native.code,
        lookup.wordClass,
        lookup.word,
      ).mkString(":"),
      false,
    )

  /**
    * Runs programs by name, so that one run while another of its name runs
    * waits for that one, and has what it gives, rather than running.
    */
  private final class Once[A]:

    private val running = Ref
      .in[
        SyncIO,
        IO,
        Map[
          String,
          Deferred[IO, Either[Throwable, A]],
        ],
      ](Map.empty)
      .unsafeRunSync()

    /**
      * What `program` gives, or, if another of `name` is running, what that one
      * gives. Between its being noted and its starting, the first cannot be
      * cancelled, lest those that wait for it wait for ever.
      */
    def apply(name: String)(program: IO[A]): IO[A] =
      Deferred[IO, Either[Throwable, A]]
        .flatMap(mine =>
          IO.uncancelable(poll =>
            running
              .modify(now =>
                now.get(name) match
                  case Some(theirs) => (now, poll(theirs.get))
                  case None         => (
                      now.updated(name, mine),
                      poll(program)
                        .onCancel(
                          mine
                            .complete(Left(Problem("The lookup was stopped.")))
                            .void,
                        )
                        .attempt
                        .flatTap(mine.complete)
                        .guarantee(running.update(_ - name)),
                    ),
              )
              .flatten,
          ),
        )
        .rethrow

  /** The lexicon being opened. */
  private val opening = Once[Store]()

  /** The words being fetched, by their URLs. */
  private val fetching = Once[Unit]()

  /** The words whose texts are being put into the reader's language. */
  private val translating = Once[List[Lexeme]]()

  /** The words being generated. */
  private val generating = Once[List[Lexeme]]()

  /** What lets the lexicon be changed one change at a time. */
  private val lock = Semaphore.in[SyncIO, IO](1).unsafeRunSync()

  /** How long a model is not asked again for what it failed to give. */
  val pause: FiniteDuration = 1.minute

  /** When each request of a model by name last failed, and why. */
  private val failed = Ref
    .in[
      SyncIO,
      IO,
      Map[String, (FiniteDuration, Throwable)],
    ](Map.empty)
    .unsafeRunSync()

  /** Forgets every request that failed, as the tests do between them. */
  private[codeswitch] val forgetFailures: IO[Unit] = failed.set(Map.empty)

  /**
    * What `program` gives, or why the request of a model that it makes, by
    * `name`, failed, if it did less than [[pause]] ago, so that a failing model
    * is not asked on every hover.
    */
  private def unlessFailed[A](name: String)(program: IO[A]): IO[A] =
    (IO.monotonic, failed.get).flatMapN((now, failures) =>
      failures
        .get(name)
        .filter((at, _) => now - at < pause)
        .fold(program)((_, error) => IO.raiseError(error)),
    )

  /**
    * `request` of a model, by `name`, keeping why it fails, if it does, for
    * [[unlessFailed]]. Only a request made is kept, so that a reader who had no
    * key has one used as soon as they give it.
    */
  private def noting[A](name: String)(request: IO[A]): IO[A] = request.onError {
    case error => IO
        .monotonic
        .flatMap(now =>
          failed.update(failures =>
            failures
              .filter((_, failure) => now - failure._1 < pause)
              .updated(name, now -> error),
          ),
        )
  }

  /**
    * The text at `url`, through the browser, which the background may ask of
    * Kaikki, as `hosts` grants it, though Kaikki does not allow requests from
    * other origins.
    */
  def fetch(url: String): IO[Option[String]] = IO
    .fromPromise(IO(dom.fetch(url)))
    .adaptError(_ => Problem("Wiktionary could not be reached."))
    .flatMap(read)

  /**
    * The text of `response`, or none if there is none there (404), or a failure
    * if it cannot be had, as when Kaikki is busy, so that nothing is kept of
    * it.
    */
  private[codeswitch] def read(response: dom.Response): IO[Option[String]] =
    if response.ok then IO.fromPromise(IO(response.text())).map(Some(_))
    else if response.status == 404 then IO.none
    else
      IO.raiseError(Problem(s"Wiktionary could not be read (${ response
          .status }). Try again soon."))
