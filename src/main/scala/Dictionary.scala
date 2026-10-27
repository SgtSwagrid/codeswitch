package com.alecdorrington.codeswitch

import cats.effect.{Deferred, IO, Ref, SyncIO}
import cats.effect.std.Semaphore
import cats.syntax.all.*
import com.alecdorrington.iris.LlmClient
import com.alecdorrington.omnibrowser.{Browser, StorageKey}
import io.circe.Json
import io.circe.syntax.*
import org.scalajs.dom

/**
  * Looks words up for the card: in Wiktionary, through Kaikki, wherever it has
  * them, and otherwise from a model, keeping what it finds on this device. Only
  * the background uses it, which keeps Iris out of the pages.
  */
object Dictionary:

  /**
    * Runs a program with a client of the reader's model, asked for only when a
    * model is needed, as it needs the reader's key.
    */
  type Ask = [A] => (LlmClient[IO] => IO[A]) => IO[A]

  /**
    * The text at a URL, or none if there is none there, or a failure if it
    * cannot be had.
    */
  type Fetch = String => IO[Option[String]]

  /** How many of the entries kept on this device are kept, the latest used. */
  val capacity = 300

  /** The names of the entries kept on this device, the latest used first. */
  val kept = StorageKey.local[List[String]]("dictionary", Nil)

  /**
    * Wiktionary's entries for `word`, of `learnt`, once fetched, which are none
    * if it has none.
    */
  def wiktionary
    (learnt: Language, word: String)
    : StorageKey[Option[List[Entry]]] = StorageKey.local(
    s"wiktionary:${ learnt.code }:$word",
    None,
  )

  /**
    * The `index`th of Wiktionary's entries for `word`, of the language that
    * `learner` is learning, put into their own. The word comes last in this
    * name, and the others, so that no word can be taken for another's.
    */
  def explained
    (learner: Learner, index: Int, word: String)
    : StorageKey[Option[Entry]] = StorageKey.local(
    List(
      "explained",
      learner.language.code,
      learner.native.code,
      index.toString,
      word,
    ).mkString(":"),
    None,
  )

  /**
    * A model's entry for `lookup`, for `learner`, if it made one, once asked:
    * kept even if it made none, as asking again would cost as much each time.
    */
  def made(learner: Learner, lookup: Lookup): StorageKey[Option[List[Entry]]] =
    StorageKey.local(
      List(
        "made",
        learner.language.code,
        learner.native.code,
        lookup.pos,
        lookup.word,
      ).mkString(":"),
      None,
    )

  /**
    * The entry for `lookup`, a word of the language that `learner` is learning,
    * explained in their own, as kept in `storage`, or else as found, then kept:
    * Wiktionary's, from `fetch`, of the word's part of speech, with the gloss
    * that best fits its meaning, its senses put into the reader's language by a
    * model if that is not English; or else, if Wiktionary has none, one that a
    * model makes.
    *
    * If a model cannot put Wiktionary's entries into the reader's language, as
    * when they have given no key, they are given in English, and not kept, so
    * that they are put into it once it can. A lookup made again while it is
    * being looked up waits for it, rather than asking again.
    */
  def entry
    (
      learner: Learner,
      lookup: Lookup,
      storage: Browser.Storage,
      fetch: Fetch,
      ask: Ask,
    )
    : IO[Option[Entry]] = once(
    List(
      learner.language.code,
      learner.native.code,
      lookup.asJson.noSpaces,
    ).mkString(":"),
  )(
    entries(
      learner.language,
      lookup.word,
      storage,
      fetch,
    ).map(Wiktionary.candidates(_, lookup))
      .flatMap {
        case Nil => modelled(learner, lookup, storage, ask)
        case found if learner.native == Language.English =>
          IO.pure(Wiktionary.choose(found.map(_._1), lookup.meaning))
        case found => translated(learner, lookup.word, found, storage, ask).map(
            Wiktionary.choose(_, lookup.meaning),
          )
      },
  )

  /** The lookups being made, by what they look up, each to be had once made. */
  private val looking = Ref
    .in[
      SyncIO,
      IO,
      Map[
        String,
        Deferred[IO, Either[Throwable, Option[Entry]]],
      ],
    ](Map.empty)
    .unsafeRunSync()

  /**
    * What `lookup` finds, or, if another lookup of `name` is being made, what
    * that one finds.
    */
  private def once(name: String)(lookup: IO[Option[Entry]]): IO[Option[Entry]] =
    Deferred[IO, Either[Throwable, Option[Entry]]]
      .flatMap(mine =>
        looking
          .modify(running =>
            running.get(name) match
              case Some(theirs) => (running, theirs.get)
              case None         => (
                  running.updated(name, mine),
                  lookup
                    .attempt
                    .flatTap(mine.complete)
                    .onCancel(
                      mine.complete(Left(Problem("Not looked up."))).void,
                    )
                    .guarantee(looking.update(_ - name)),
                ),
          )
          .flatten,
      )
      .rethrow

  /** Wiktionary's entries for `word`, of `learnt`, as kept, or else fetched. */
  private def entries
    (
      learnt: Language,
      word: String,
      storage: Browser.Storage,
      fetch: Fetch,
    )
    : IO[List[Entry]] =
    val key = wiktionary(learnt, word)
    recall(storage, key).flatMap(
      _.fold {
        val url = Wiktionary.url(learnt, word)
        fetch(url)
          .map(_.fold(Nil)(Wiktionary.entries(_, url)))
          .flatTap(keep(storage, key, _))
      }(IO.pure),
    )

  /**
    * Each of `found`, Wiktionary's entries for `word`, each with its place
    * among them, put into `learner`'s language, as kept, or else by a model,
    * all at once, then kept, or else as it is.
    */
  private def translated
    (
      learner: Learner,
      word: String,
      found: List[(Entry, Int)],
      storage: Browser.Storage,
      ask: Ask,
    )
    : IO[List[Entry]] =
    for
      kept <- found.traverse((_, index) =>
        recall(storage, explained(learner, index, word)),
      )
      missing = found.zip(kept).collect { case (entry, None) => entry }
      fresh <-
        if missing.isEmpty then IO.pure(Map.empty[Int, Entry])
        else
          ask(Translator.explained(_, learner, missing.map(_._1)))
            .attempt
            .map(
              _.toOption.flatten.fold(Map.empty)(missing.map(_._2).zip(_).toMap),
            )
            .flatTap(
              _.toList
                .traverse_((index, entry) =>
                  keep(
                    storage,
                    explained(learner, index, word),
                    entry,
                  ),
                ),
            )
    yield found
      .zip(kept)
      .map { case ((entry, index), kept) =>
        kept.orElse(fresh.get(index)).getOrElse(entry)
      }

  /** A model's entry for `lookup`, for `learner`, as kept, or else made. */
  private def modelled
    (
      learner: Learner,
      lookup: Lookup,
      storage: Browser.Storage,
      ask: Ask,
    )
    : IO[Option[Entry]] =
    val key = made(learner, lookup)
    recall(storage, key).flatMap(
      _.fold(ask(Translator.defined(_, learner, lookup)).flatTap(found =>
        keep(storage, key, found.toList),
      ))(found => IO.pure(found.headOption)),
    )

  /**
    * What is kept under `key`, if anything can be read there, which is then the
    * latest used.
    */
  private def recall[A]
    (
      storage: Browser.Storage,
      key: StorageKey[Option[A]],
    )
    : IO[Option[A]] = storage
    .get(key)
    .handleError(_ => None)
    .flatTap(found => touch(storage, key.name).whenA(found.nonEmpty))

  /**
    * Keeps `value` under `key`, as the latest used, or nothing, if there is no
    * room to, so that the reader still has what was found.
    */
  private def keep[A]
    (
      storage: Browser.Storage,
      key: StorageKey[Option[A]],
      value: A,
    )
    : IO[Unit] = (storage.set(key, Some(value)) >> touch(storage, key.name))
    .handleError(_ => ())

  /**
    * Makes the entry kept as `name` the latest used, and forgets those beyond
    * [[capacity]], one change at a time, lest two that overlap lose one.
    */
  private def touch(storage: Browser.Storage, name: String): IO[Unit] = lock
    .permit
    .surround(
      storage
        .get(kept)
        .flatMap(names =>
          val (latest, beyond) = (name :: names.filterNot(_ == name)).splitAt(
            capacity,
          )
          storage.set(kept, latest) >> beyond.traverse_(old =>
            storage.remove(StorageKey.local[Json](old, Json.Null)),
          ),
        ),
    )
    .handleError(_ => ())

  private val lock = Semaphore.in[SyncIO, IO](1).unsafeRunSync()

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
