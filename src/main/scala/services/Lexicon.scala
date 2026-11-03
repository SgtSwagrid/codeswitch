package com.alecdorrington.codeswitch.services

import cats.effect.{Deferred, IO, Ref, SyncIO}
import cats.effect.std.Semaphore
import cats.syntax.all.*
import com.alecdorrington.codeswitch.lexicon.{
  Derivation, Dictionary, Homographs, Lexeme, LexemeId, Record, Source,
  Spelling, Store, Timestamp, WordClass,
}
import com.alecdorrington.codeswitch.models.{
  alike, whole, Entry, Lookup, Wiktionary,
}
import com.alecdorrington.iris.{Model, ModelClient}
import com.alecdorrington.megalingual.Language
import com.alecdorrington.omnibrowser.{Browser, StorageKey}
import org.scalajs.dom
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}

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

  /**
    * The entry for `lookup`, a word of a language being learnt, explained in
    * `native`, the reader's own, as kept in `store`, of what the dictionaries
    * that the reader has not turned off in `storage` give: Wiktionary's, from
    * `fetch`, the first time the word is looked up, of the word's class, with
    * the gloss that best fits its meaning, its texts put into the reader's
    * language by a model if that is not English; or else, if no dictionary has
    * the word, one that a model generates, as it does if the dictionaries have
    * it only in other classes than the lookup's (see [[Lookup.fits]]), as a
    * dictionary may lack one, unless the model finds it no word of that class
    * either.
    *
    * If a model cannot put Wiktionary's texts into the reader's language, as
    * when they have given no key, they are given in English, and none is kept,
    * so that they are put into it once it can. Each part of a lookup (fetching
    * a word, putting its texts into the reader's language, generating one)
    * asked for again while it is being found waits for it, rather than asking
    * again (see [[Once]]), and a model is not asked again for a minute after it
    * failed (see [[unlessFailed]]). A lexeme kept with what the reader, or a
    * model, put in it is given to `review` once its record is fetched again
    * (see [[Reviews]]).
    */
  def entry
    (
      native: Language,
      lookup: Lookup,
      store: Store,
      storage: Browser.Storage,
      fetch: Fetch,
      ask: Ask,
      review: LexemeId => IO[Unit],
    )
    : IO[Option[(LexemeId, Entry)]] = storage
    .get(off)
    .flatMap(off =>
      val shown  = (source: Source) => source.dictionaries.forall(!off(_))
      val learnt = lookup.language
      (if off(Dictionary.Wiktionary) then IO.pure(Nil)
       else relearn(learnt, lookup.word, store, fetch, review))
        .flatMap(found(learnt, lookup, native, store, shown, _))
        .flatMap {
          case Nil   => generated(native, lookup, store, storage, ask, shown)
          case found =>
            val chosen =
              if native.alike(Language.English) then
                IO.pure(choose(found, learnt, native, lookup, shown))
              else
                translated(native, lookup, found, store, ask, shown).map(
                  choose(_, learnt, native, lookup, shown),
                )
            if found.exists(lexeme =>
                Lookup.fits(lookup.wordClass)(lexeme.wordClass.name),
              )
            then chosen
            else
              generated(
                native,
                lookup,
                store,
                storage,
                ask,
                shown,
                strictly = true,
              ).flatMap(_.fold(chosen)(IO.some))
        },
    )

  /**
    * Learns `word`, of `learnt`, from Wiktionary, by `fetch`, unless its
    * spelling has been checked there already: each lexeme of Wiktionary's
    * entries for it, merged with what is kept of it, each of its spellings
    * listed among its homographs, and the record fetched, kept. That Wiktionary
    * has none of it is kept too, as asking again would find none again. It is
    * fetched by its spelling, as Wiktionary names its pages without marks of
    * stress, and gives the lexemes learnt, if it was fetched, which are kept if
    * there is room.
    */
  private[codeswitch] def learn
    (
      learnt: Language,
      word: String,
      store: Store,
      fetch: Fetch,
    )
    : IO[List[Lexeme]] =
    val spelling = Spelling(learnt, word)
    val url      = Wiktionary.url(learnt, spelling.text)
    store
      .homographs(List(spelling))
      .map(_.head.checked.contains(learnt.whole))
      .ifM(
        IO.pure(Nil),
        fetching(url)((fetch(url), Timestamp.now).flatMapN((lines, at) =>
          record(
            learnt,
            spelling,
            Source.Sourced(Dictionary.Wiktionary, url, at),
            lines,
            store,
          ).flatMap(found =>
            lines
              .fold(Nil)(Derivation.pointers(learnt, _))
              .filter(_._2 != spelling)
              .traverse((wordClass, lemma) =>
                follow(
                  learnt,
                  spelling,
                  wordClass,
                  lemma,
                  store,
                  fetch,
                ),
              )
              .map(found ++ _.flatten),
          ),
        )),
      )

  /**
    * Learns `word`, of `learnt`, from Wiktionary (see [[learn]]), giving
    * `review` each lexeme fetched again that was kept with what the reader, or
    * a model, or another record put in it, which may now repeat what the record
    * gives.
    */
  private def relearn
    (
      learnt: Language,
      word: String,
      store: Store,
      fetch: Fetch,
      review: LexemeId => IO[Unit],
    )
    : IO[List[Lexeme]] = spelt(store, Spelling(learnt, word)).flatMap(before =>
    learn(learnt, word, store, fetch).flatTap(fresh =>
      fresh
        .filter(lexeme => before.find(_.id == lexeme.id).exists(held(lexeme)))
        .traverse_(lexeme => review(lexeme.id)),
    ),
  )

  /** The lexemes kept that are spelt `spelling`. */
  private def spelt(store: Store, spelling: Spelling): IO[List[Lexeme]] = store
    .homographs(List(spelling))
    .flatMap(homographs => store.lexemes(homographs.head.lexemes))

  /**
    * Whether `before` held a claim from another source than the records that
    * `lexeme` was fetched from again.
    */
  private def held(lexeme: Lexeme)(before: Lexeme): Boolean = before
    .claims
    .exists(claim => !Derivation.recordOf(claim.source).exists(lexeme.records))

  /**
    * Learns `lemma`, of `learnt`, which Wiktionary gives `spelling` as a form
    * or another spelling of, a `wordClass`, and lists among `spelling`'s
    * homographs the lexemes of `lemma` of that class, or every one of `lemma`'s
    * if none is, giving them, so that `ging` is counted and looked up as
    * `gehen`.
    */
  private def follow
    (
      learnt: Language,
      spelling: Spelling,
      wordClass: WordClass,
      lemma: Spelling,
      store: Store,
      fetch: Fetch,
    )
    : IO[List[Lexeme]] = learn(learnt, lemma.text, store, fetch) *>
    lock
      .permit
      .surround(
        store
          .homographs(List(lemma, spelling))
          .flatMap(listed =>
            store
              .lexemes(listed.head.lexemes)
              .flatMap(lexemes =>
                val lemmas = lexemes.filter(lexeme =>
                  lexeme.lemma.spelling == lemma &&
                  lexeme.languages.exists(_.whole == learnt.whole),
                )
                val fitting = Some(lemmas.filter(_.wordClass == wordClass))
                  .filter(_.nonEmpty)
                  .getOrElse(lemmas)
                store
                  .keep(
                    Nil,
                    List(listed(1).including(fitting.map(_.id))),
                    Nil,
                  )
                  .handleError(_ => ())
                  .as(fitting),
              ),
          ),
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
    * overlap lose one, or nothing, if there is no room to, so that the reader
    * still has what was found. It gives the lexemes as merged.
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
        homographs <- listing(store, merged, spellings, change)
        _ <- store.keep(merged, homographs, records).handleError(_ => ())
      yield merged,
    )

  /**
    * The homographs of each spelling of `lexemes` (see [[Lexeme.spellings]]),
    * and of `spellings`, each listing the lexemes spelt so, as `change` changes
    * it.
    */
  private[services] def listing
    (
      store: Store,
      lexemes: List[Lexeme],
      spellings: List[Spelling],
      change: Homographs => Homographs,
    )
    : IO[List[Homographs]] =
    val spelt =
      lexemes.flatMap(lexeme => lexeme.spellings.map(_.spelling -> lexeme.id))
    store
      .homographs((spellings ++ spelt.map(_._1)).distinct)
      .map(_.map(homographs =>
        change(homographs.including(spelt.collect {
          case (spelling, id) if spelling == homographs.spelling => id
        })),
      ))

  /**
    * The lexemes kept of `lookup`'s word, spelt alike in `learnt`, or, where
    * none is the word itself, those it is a form or another spelling of, as
    * `ging` is of `gehen`, that a card may show a reader of `native`, of only
    * what `shown` lets it show: each with senses, of the word's class, if any
    * are, as when a word is two words, as `Bank`, a bench, and `Bank`, a bank
    * (see [[Wiktionary.candidates]]), and only those with a dictionary's
    * senses, if any have, as a model generates a word only where no dictionary
    * has it. Those just `learnt` are among them, though there was no room to
    * keep them.
    */
  private def found
    (
      learnt: Language,
      lookup: Lookup,
      native: Language,
      store: Store,
      shown: Source => Boolean,
      fresh: List[Lexeme],
    )
    : IO[List[Lexeme]] =
    val spelling = Spelling(learnt, lookup.word)
    store
      .homographs(List(spelling))
      .flatMap(homographs => store.lexemes(homographs.head.lexemes))
      .map(kept => (fresh ++ kept).distinctBy(_.id))
      .map(lexemes =>
        val known  = lexemes.filter(_.languages.exists(_.whole == learnt.whole))
        val lemmas = known.filter(_.lemma.spelling == spelling)
        val alike  = if lemmas.nonEmpty then lemmas else known
        val classed = alike.filter(lexeme =>
          Lookup.fits(lookup.wordClass)(lexeme.wordClass.name),
        )
        val fitting = if classed.nonEmpty then classed else alike
        val sourced = fitting.filter(Derivation.sourced(_, shown))
        val spelt   = if sourced.nonEmpty then sourced else fitting
        Wiktionary
          .candidates(
            spelt.map(Derivation.entry(_, learnt, native, shown)),
            lookup,
          )
          .map((_, index) => spelt(index)),
      )

  /**
    * The entry of `found` that a card shows a reader of `native`, of the one
    * whose glosses best fit the meaning of `lookup`, with its own.
    */
  private def choose
    (
      found: List[Lexeme],
      learnt: Language,
      native: Language,
      lookup: Lookup,
      shown: Source => Boolean,
    )
    : Option[(LexemeId, Entry)] =
    val entries = found.map(Derivation.entry(_, learnt, native, shown))
    Wiktionary
      .choose(entries, lookup.meaning)
      .map(entry => found(entries.indexOf(entry)).id -> entry)

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
                      val translated = missing
                        .map(lexeme =>
                          kept.find(_.id == lexeme.id).getOrElse(lexeme),
                        )
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
                      store
                        .keep(translated, Nil, Nil)
                        .handleError(_ => ())
                        .as(translated),
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
    * kept too, as asking again would cost as much each time. It is kept as the
    * word looked up, though the model wrote it otherwise (`sich freuen` for
    * `freuen`), so that the next lookup finds it, and asks no model again. If
    * it is asked for `strictly`, as a dictionary has the word only in another
    * class, an entry of another class than the lookup's counts as none, as the
    * model then found the word to be no word of that class.
    */
  private def generated
    (
      native: Language,
      lookup: Lookup,
      store: Store,
      storage: Browser.Storage,
      ask: Ask,
      shown: Source => Boolean,
      strictly: Boolean = false,
    )
    : IO[Option[(LexemeId, Entry)]] =
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
            case (_, Some(entry))
              if strictly && !Lookup.fits(lookup.wordClass)(entry.wordClass) =>
              storage.set(key, true).handleError(_ => ()).as(Nil)
            case (model, Some(entry)) => Timestamp
                .now
                .flatMap(at =>
                  keeping(
                    store,
                    List(Derivation.generated(
                      entry.copy(word = lookup.word),
                      learnt,
                      native,
                      model,
                      at,
                    )),
                    Nil,
                    identity,
                    Nil,
                  ),
                )
          },
        ).map(
          _.headOption
            .map(lexeme =>
              lexeme.id -> Derivation.entry(lexeme, learnt, native, shown),
            )
            .filter(_._2.senses.nonEmpty),
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
  private[services] final class Once[A]:

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

  /** The words being fetched, by their URLs. */
  private val fetching = Once[List[Lexeme]]()

  /** The words whose texts are being put into the reader's language. */
  private val translating = Once[List[Lexeme]]()

  /** The words being generated. */
  private val generating = Once[List[Lexeme]]()

  /** What lets the lexicon be changed one change at a time. */
  private[services] val lock = Semaphore.in[SyncIO, IO](1).unsafeRunSync()

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
  def fetch(url: String): IO[Option[String]] = request(url)
    .adaptError(_ => Problem("Wiktionary could not be reached."))
    .flatMap(read)

  /** The response to a request of a URL, or a failure if none came. */
  type Request = String => IO[dom.Response]

  /** The response to a request of `url`, through the browser. */
  def request(url: String): IO[dom.Response] =
    IO.fromPromise(IO(dom.fetch(url)))

  /** The largest recording that is played, as a word's takes a second or two. */
  val largest: Int = 1 << 20

  /**
    * The recording at `url`, on Wikimedia Commons, as `request` gives it, its
    * bytes in Base64, as the page that plays it is sent them, or a failure if
    * it cannot be had, or is not a recording there of at most [[largest]]
    * bytes, as a page's content script, which asks, may be the page's.
    */
  def recording(url: String, request: Request): IO[String] = IO.raiseUnless(
    Wiktionary.recorded(url),
  )(Problem("That is not a recording on Wikimedia Commons.")) *> request(url)
    .adaptError(_ => Problem("Wikimedia Commons could not be reached."))
    .flatMap(response =>
      IO.raiseUnless(audible(response))(Problem(
        s"The recording could not be had (${ response.status }).",
      )) *> IO.fromPromise(IO(response.arrayBuffer())),
    )
    .flatMap(bytes =>
      IO.raiseWhen(bytes.byteLength > largest)(Problem(
          "The recording is too long.",
        ))
        .as(base64(bytes)),
    )

  /**
    * Whether `response` is of a recording, of at most [[largest]] bytes, if it
    * says how many.
    */
  private def audible(response: dom.Response): Boolean = response.ok && Option(
    response.headers.get("content-type"),
  ).exists(_.startsWith("audio/")) &&
    Option(response.headers.get("content-length"))
      .flatMap(_.toIntOption)
      .forall(_ <= largest)

  /** `bytes` in Base64. */
  private def base64(bytes: ArrayBuffer): String =
    val each = Uint8Array(bytes)
    btoa((0 until each.length).map(index => each(index).toChar).mkString)

  /** `binary`, a string of one byte in each character, in Base64. */
  @js.native
  @JSGlobal("btoa")
  private def btoa(binary: String): String = js.native

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
