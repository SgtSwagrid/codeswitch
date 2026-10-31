package com.alecdorrington.codeswitch

import cats.effect.{Deferred, IO, Ref, SyncIO}
import cats.effect.std.Semaphore
import cats.syntax.all.*
import com.alecdorrington.iris.LlmClient
import com.alecdorrington.megalingual.Language
import com.alecdorrington.omnibrowser.{Browser, StorageKey}
import io.circe.Json
import org.scalajs.dom
import scala.concurrent.duration.{DurationInt, FiniteDuration}

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
    * if it has none. Each is kept by the whole language, as Wiktionary has it,
    * not its variety, but in the reader's own variety, as it explains in it.
    */
  def wiktionary
    (learnt: Language, word: String)
    : StorageKey[Option[List[Entry]]] = StorageKey.local(
    s"wiktionary:${ learnt.whole.code }:$word",
    None,
  )

  /**
    * The `index`th of Wiktionary's entries for `word`, of `learnt`, put into
    * `native`, the reader's own language. The word comes last in this name, and
    * the others, so that no word can be taken for another's.
    */
  def explained
    (
      learnt: Language,
      native: Language,
      index: Int,
      word: String,
    )
    : StorageKey[Option[Entry]] = StorageKey.local(
    List(
      "explained",
      learnt.whole.code,
      native.code,
      index.toString,
      word,
    ).mkString(":"),
    None,
  )

  /**
    * A model's entry for `lookup`, in `native`, the reader's own language, if
    * it made one, once asked: kept even if it made none, as asking again would
    * cost as much each time.
    */
  def made(native: Language, lookup: Lookup): StorageKey[Option[List[Entry]]] =
    StorageKey.local(
      List(
        "made",
        lookup.language.whole.code,
        native.code,
        lookup.pos,
        lookup.word,
      ).mkString(":"),
      None,
    )

  /**
    * The entry for `lookup`, a word of a language being learnt, explained in
    * `native`, the reader's own, as kept in `storage`, or else as found, then
    * kept: Wiktionary's, from `fetch`, of the word's part of speech, with the
    * gloss that best fits its meaning, its senses put into the reader's
    * language by a model if that is not English; or else, if Wiktionary has
    * none, one that a model makes.
    *
    * If a model cannot put Wiktionary's entries into the reader's language, as
    * when they have given no key, they are given in English, and not kept, so
    * that they are put into it once it can. Each part of a lookup (fetching a
    * word's entries, putting them into the reader's language, a model's making
    * one) asked for again while it is being found waits for it, rather than
    * asking again (see [[Once]]), and a model is not asked again for a minute
    * after it failed (see [[unlessFailed]]).
    */
  def entry
    (
      native: Language,
      lookup: Lookup,
      storage: Browser.Storage,
      fetch: Fetch,
      ask: Ask,
    )
    : IO[Option[Entry]] = entries(
    lookup.language,
    lookup.word,
    storage,
    fetch,
  ).map(Wiktionary.candidates(_, lookup))
    .flatMap {
      case Nil => modelled(native, lookup, storage, ask)
      case found if native.alike(Language.English) =>
        IO.pure(Wiktionary.choose(found.map(_._1), lookup.meaning))
      case found => translated(native, lookup, found, storage, ask).map(
          Wiktionary.choose(_, lookup.meaning),
        )
    }

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
    once(key.name)(recall(storage, key).flatMap(
      _.fold {
        val url = Wiktionary.url(learnt, word)
        fetch(url)
          .map(_.fold(Nil)(Wiktionary.entries(learnt, _, url)))
          .flatTap(keep(storage, key, _))
      }(IO.pure),
    ))

  /**
    * Each of `found`, Wiktionary's entries for the word of `lookup`, each with
    * its place among them, put into `native`, the reader's own language, as
    * kept, if it is still of the entry in that place, or else by a model, all
    * at once, then kept, or else as it is.
    */
  private def translated
    (
      native: Language,
      lookup: Lookup,
      found: List[(Entry, Int)],
      storage: Browser.Storage,
      ask: Ask,
    )
    : IO[List[Entry]] =
    val key = explained(lookup.language, native, _, lookup.word)
    for
      kept <- found.traverse((entry, index) =>
        recall(storage, key(index)).map(_.filter(fits(entry))),
      )
      missing = found.zip(kept).collect { case (entry, None) => entry }
      fresh <-
        if missing.isEmpty then IO.pure(Map.empty[Int, Entry])
        else
          val name = missing.map((_, index) => key(index).name).mkString(",")
          once(name)(
            unlessFailed(name)(ask(client =>
              noting(name)(
                Translator
                  .explained(
                    client,
                    lookup.language,
                    native,
                    missing.map(_._1),
                  )
                  .flatMap(IO.fromOption(_)(Problem(
                    "Wiktionary's entries could not be put into your language.",
                  ))),
              ),
            )).flatTap(entries =>
              missing
                .zip(entries)
                .traverse_((place, entry) =>
                  keep(storage, key(place._2), entry),
                ),
            ),
          ).attempt
            .map(_.fold(
              _ => Map.empty,
              entries => missing.map(_._2).zip(entries).toMap,
            ))
    yield found
      .zip(kept)
      .map { case ((entry, index), kept) =>
        kept.orElse(fresh.get(index)).getOrElse(entry)
      }

  /**
    * Whether `kept`, kept as `entry` put into the reader's language, is still
    * of it, as Wiktionary's entries for a word may since have changed places.
    */
  private def fits(entry: Entry)(kept: Entry): Boolean = kept.word ==
    entry.word && kept.pos == entry.pos && kept.senses.size == entry.senses.size

  /**
    * A model's entry for `lookup`, in `native`, the reader's own language, as
    * kept, or else made.
    */
  private def modelled
    (
      native: Language,
      lookup: Lookup,
      storage: Browser.Storage,
      ask: Ask,
    )
    : IO[Option[Entry]] =
    val key = made(native, lookup)
    once(key.name)(recall(storage, key).flatMap(
      _.fold(
        unlessFailed(key.name)(ask(client =>
          noting(key.name)(Translator.defined(client, native, lookup)),
        )).map(_.toList).flatTap(keep(storage, key, _)),
      )(IO.pure),
    )).map(_.headOption)

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

  /** The parts of the lookups being made, each of what it is kept as. */
  private val once = Once[List[Entry]]()

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
    .flatTap(found =>
      touch(storage, key.name).handleError(_ => ()).whenA(found.nonEmpty),
    )

  /**
    * Keeps `value` under `key`, as the latest used, or nothing, if there is no
    * room to, so that the reader still has what was found. Its name is listed
    * first, so that nothing is kept that is not listed, and so forgotten.
    */
  private def keep[A]
    (
      storage: Browser.Storage,
      key: StorageKey[Option[A]],
      value: A,
    )
    : IO[Unit] = (touch(storage, key.name) >> storage.set(key, Some(value)))
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
