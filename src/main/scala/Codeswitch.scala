package com.alecdorrington.codeswitch

import cats.Eq
import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.alecdorrington.codeswitch.models.{
  Card, Familiarity, Foreign, Headword, Hint, Learner, Learnt, Lookup, Passages,
  Rating, Scope, Site, Switch, Translation, Wiktionary, Word, Words,
}
import com.alecdorrington.codeswitch.services.{
  Arrival, Arrivals, FamiliarityService, FrequencyService, Lexicon,
  PriorityService, Problem, SpendingService, Streamed, Translator,
}
import com.alecdorrington.codeswitch.services.Translator.model
import com.alecdorrington.codeswitch.views.{Asking, Popup, Reader, Tips}
import com.alecdorrington.iris.{Model, ModelClient, ModelProvider}
import com.alecdorrington.megalingual.Language
import com.alecdorrington.omnibrowser.*
import com.alecdorrington.omnibrowser.Permission.Storage
import com.alecdorrington.omnibrowser.megalingual.{languages, speech}
import fs2.Stream
import fs2.concurrent.SignallingRef
import io.circe.{Codec, Decoder, Encoder, Json}
import org.scalajs.dom.{console, window}
import scala.concurrent.duration.DurationInt

/**
  * Teaches you a language by translating a few words of every page you read.
  * Each word of a page in a language that the reader reads fluently has a
  * chance, the [[intensity]] that they choose, of being shown in one of the
  * languages they are learning, highlighted, with the original word shown above
  * it on hovering over it. In a page in a language being learnt, the words a
  * level above the reader's are highlighted, with their meanings shown above
  * them, and harder words are shown as their meanings, in the reader's own
  * language.
  */
object Codeswitch extends BrowserExtension[Storage]:

  override val name = "Codeswitch"

  override val description =
    "Teaches you a language by translating a few words of every page you read."

  override val icons = Seq(16, 32, 48, 128)
    .map(size => size -> s"icons/icon-$size.png")
    .toMap

  // The words to translate, with the text around them, and the passages to
  // read, go to the provider, the words looked up to Kaikki, and the
  // recordings that the reader asks to hear are fetched from Wikimedia Commons.
  override val dataCollection = DataCollection(Set(PersonalData.WebsiteContent))

  // So that the background may ask each provider, and Kaikki for Wiktionary's
  // entries, whether or not each allows requests from the extension's origin.
  // The pages that the content script runs in already cover Kaikki's, so its
  // host asks the reader for nothing more when they update. Wikimedia Commons,
  // which has the recordings, allows requests from any origin.
  override def hosts = Wiktionary.host +:
    ModelProvider.values.toSeq.map(provider => s"${ provider.origin }/*")

  // Every page loads what this object's own values reach, so the key of the
  // model, whose codec and default reach Iris's models, is on `Translator`.

  /**
    * A language, if any, kept by its code. A code no language has, such as one
    * stored by an earlier release, reads as none, so the reader chooses again
    * rather than the plugin failing to read its settings.
    */
  private given Codec[Option[Language]] = Codec.from(
    Decoder[Option[String]].map(_.flatMap(Language.fromCode)),
    Encoder[Option[Language]],
  )

  /**
    * A list, of which an item that cannot be read, as of a language that no
    * code names now, is left out, rather than the plugin failing to read its
    * settings.
    */
  private def lenient[A : {Decoder, Encoder}]: Codec[List[A]] = Codec.from(
    Decoder[List[Json]].map(_.flatMap(_.as[A].toOption)),
    Encoder.encodeList[A],
  )

  private given languages: Codec[List[Language]] = lenient

  private given learnt: Codec[List[Learnt]] = lenient

  private given sites: Codec[List[Site]] = lenient

  /**
    * The reader's own language, in which words are explained, if they have
    * chosen it, or else the browser's first that is not being learnt (see
    * [[models.Learner.own]]).
    */
  val native = StorageKey.sync[Option[Language]]("native", None)

  /**
    * The languages, besides their own, that the reader reads fluently, whose
    * pages, as those in their own, have a share of their words shown in the
    * languages being learnt.
    */
  val fluent = StorageKey.sync[List[Language]]("fluent", Nil)

  /**
    * The languages that the reader is learning, each with how well they know
    * it, whose pages have their words above that level marked.
    */
  val learning = StorageKey.sync[List[Learnt]]("learning", Nil)

  /**
    * Whether passages in a foreign language, one neither read fluently nor
    * being learnt, are translated whole into the reader's own, or left as they
    * are.
    */
  val foreign = StorageKey.sync[Foreign]("foreign", Foreign.Translate)

  /**
    * The share of the words of each page to translate, from `0` up to
    * [[models.Words.maxIntensity]].
    */
  val intensity = StorageKey.sync[Double]("intensity", Words.defaultIntensity)

  /**
    * Which websites have anything marked: every one but those [[blocked]], or
    * only those [[allowed]].
    */
  val scope = StorageKey.sync[Scope]("scope", Scope.Except)

  /**
    * The websites, and parts of them, on which nothing is marked while every
    * other is, each covering its subdomains and the pages under it (see
    * [[models.Site.covers]]).
    */
  val blocked = StorageKey.sync[List[Site]]("blocked", Nil)

  /** The websites, and parts of them, on which alone anything is marked. */
  val allowed = StorageKey.sync[List[Site]]("allowed", Nil)

  /** The reader's API keys, by the key of the provider each is for. */
  val apiKeys = StorageKey.local[Map[String, String]]("apiKeys", Map.empty)

  /**
    * Why the last request for a tab's page, for translations or for hints,
    * failed, or nothing if it did not, which the page's content script shares,
    * and the popup shows for its tab. It lasts only as long as the page does.
    */
  val problem = Shared.tab[String]("problem", "")

  /** Forgets why a page's last translation failed, as the reader has acted. */
  val dismiss = Message.tab[Unit, Unit]("dismiss")

  /**
    * The address of a tab's page, which its content script gives, even while
    * nothing of the page is marked.
    */
  val address = Message.tab[Unit, String]("address")

  /**
    * Translates words, in their contexts, each into a language being learnt, or
    * fails with why it could not, each translation sent to the page as
    * [[translated]] as soon as the model has written it.
    */
  val translate = Message[
    Streamed[List[Word]],
    List[Translation],
  ]("translate")

  /**
    * Finds the words of passages in a language being learnt that are above the
    * reader's level, in each, or fails with why it could not, each passage's
    * sent to the page as [[hinted]] as soon as the model has written them.
    */
  val hints = Message[Streamed[Passages], List[List[Hint]]]("hints")

  /** A translation of a word that the page asked for, before the rest. */
  val translated = Message.tab[Arrival[Translation], Unit]("translated")

  /** The hints of a passage that the page asked about, before the rest. */
  val hinted = Message.tab[Arrival[List[Hint]], Unit]("hinted")

  /**
    * Translates passages in a foreign language whole into the reader's own,
    * each given in parts, the texts of its text nodes, and each given back in
    * as many, or none if it could not be, or fails with why it could not, each
    * passage's sent to the page as [[rendered]] as soon as the model has
    * written it.
    */
  val render = Message[
    Streamed[List[List[String]]],
    List[List[String]],
  ]("render")

  /** The translation of a passage that the page asked for, before the rest. */
  val rendered = Message.tab[Arrival[List[String]], Unit]("rendered")

  /**
    * The card of a word that the reader looks up, if the dictionary has an
    * entry for it, or fails with why it could not be had.
    */
  val define = Message[Lookup, Option[Card]]("define")

  /**
    * The recording of a word that the reader asks to hear, at an address on
    * Wikimedia Commons, its bytes in Base64, or fails with why it could not be
    * had.
    */
  val hear = Message[String, String]("hear")

  /**
    * Keeps the reader's rating of a word of a language being learnt, which only
    * the background writes (see [[services.FamiliarityService]]).
    */
  val rate = Message[Rating, Unit]("rate")

  /**
    * Keeps that the reader has seen words of languages being learnt shown on a
    * page, which only the background writes (see
    * [[services.FamiliarityService]]).
    */
  val seen = Message[List[Headword], Unit]("seen")

  override def background() = Background(
    onInstalled((_, browser) => retire(browser.storage)),
    onStartup(browser => retire(browser.storage)),
    serve(translate)((asked, sender, browser) =>
      reporting(browser.action, sender.tab.map(_.id))(
        translations(asked.request, browser.storage)(sending(
          browser.tabs,
          sender,
          translated,
          asked.id,
        )),
      ),
    ),
    serve(hints)((asked, sender, browser) =>
      reporting(browser.action, sender.tab.map(_.id))(
        hintsFor(asked.request, browser.storage)(
          sending(browser.tabs, sender, hinted, asked.id),
        ),
      ),
    ),
    serve(render)((asked, sender, browser) =>
      reporting(browser.action, sender.tab.map(_.id))(
        renderings(asked.request, browser.storage)(
          sending(browser.tabs, sender, rendered, asked.id),
        ),
      ),
    ),
    serve(define)((lookup, browser) =>
      defining(lookup, browser.storage).adaptError { case error =>
        Problem(Translator.explain(error))
      },
    ),
    serve(hear)((url, _) => Lexicon.recording(url, Lexicon.request)),
    serve(rate)((rating, browser) => rated(rating, browser.storage)),
    serve(seen)((shown, browser) => sighted(shown, browser.storage)),
  )

  override def contentScripts() = Seq(
    ContentScript("http://*/*", "https://*/*")(browser =>
      browser
        .serve(address)(_ => page)
        .surround(
          ignoring(browser.storage).find(!_).compile.drain *> marking(browser),
        ),
    ).withCss("content.css"),
  )

  /**
    * Marks the page for the reader, as they choose, while it is not left alone,
    * sharing why the last request failed.
    */
  private def marking(browser: TabBrowser): IO[Unit] =
    val storage = browser.storage
    (
      SignallingRef[IO].of(""),
      Arrivals[Translation],
      Arrivals[List[Hint]],
      Arrivals[List[String]],
    ).flatMapN(
      (
        problem,
        translations,
        hintsOf,
        renditions,
      ) =>
        (Tips.shown(
          rating => browser.run(browser.runtime.call(rate)(rating)),
          unlessIgnored(storage)((lookup: Lookup) =>
            browser.runtime.call(define)(lookup).attempt.map(_.left.map(reason)),
          )(_ => Right(None)),
          priorities(storage).standing,
          browser.runtime.call(hear),
          browser.speech,
          browser.run,
        ) <* offering(browser, problem) <*
          browser.serve(translated)(translations.arrived) <*
          browser.serve(hinted)(hintsOf.arrived) <*
          browser.serve(rendered)(renditions.arrived)).use(tips =>
          Reader.read(
            readers(storage),
            storage.values(intensity),
            Asking(
              (words, each) =>
                unlessIgnored(storage)(noting(problem)(
                  calling(browser, translate, translations)(_, each),
                ))(_.map(_ => Translation.declined))(words),
              (passages, each) =>
                unlessIgnored(storage)(
                  hinting(problem)(calling(browser, hints, hintsOf)(_, each)),
                )(_.texts.map(_ => Nil))(passages),
              (passages, each) =>
                unlessIgnored(storage)(rendering(problem)(
                  calling(browser, render, renditions)(_, each),
                ))(_.map(_ => Nil))(passages),
              browser.languages.detect,
              priorities(storage).of,
              priorities(storage).ofForms,
              // Keeping a sighting never stops the page being read.
              browser
                .runtime
                .call(seen)(_)
                .handleErrorWith(error =>
                  IO(console.warn(s"Codeswitch: ${ reason(error) }")),
                ),
            ),
            tips,
          ),
        ),
    )

  /**
    * What `ask` gives for each request, unless the page, at its address then,
    * is left alone, when `none` is given in its place, unasked, so that nothing
    * of a page left alone leaves the browser, even in the moment before the
    * reader hears that a single-page app moved to it.
    */
  private def unlessIgnored[A, B]
    (storage: Browser.Storage)
    (ask: A => IO[B])
    (none: A => B)
    : A => IO[B] =
    request => ignores(storage).ifM(IO.pure(none(request)), ask(request))

  /**
    * The one source of how well the reader knows each word (see
    * [[services.FamiliarityService]]), as kept in `storage`.
    */
  private[codeswitch] def familiarities
    (storage: Browser.Storage)
    : FamiliarityService = FamiliarityService(storage, storage.get(learning))

  /**
    * The one source of how often the reader has met each word (see
    * [[services.FrequencyService]]), as kept in `storage`.
    */
  private[codeswitch] def frequencies
    (storage: Browser.Storage)
    : FrequencyService = FrequencyService(storage)

  /**
    * How much the reader should focus on learning each word (see
    * [[services.PriorityService]]), as kept in `storage`.
    */
  private[codeswitch] def priorities
    (storage: Browser.Storage)
    : PriorityService = PriorityService(
    familiarities(storage),
    frequencies(storage),
  )

  /**
    * Keeps `rating`, of a word of a language being learnt, in that language as
    * the reader stored it, but not of any other, as a page's content script may
    * name, so that a stale or rogue page cannot rate words of other languages,
    * nor one that [[models.Familiarity.checked]] refuses.
    */
  private def rated(rating: Rating, storage: Browser.Storage): IO[Unit] =
    learner(storage).flatMap(reader =>
      (Familiarity.checked(rating), reader.learnt(rating.language))
        .tupled
        .traverse_((rating, learnt) =>
          familiarities(storage).rate(rating.copy(language = learnt.language)),
        ),
    )

  /**
    * Keeps that the reader has seen the first [[models.Familiarity.most]] of
    * `shown`, those of languages being learnt that
    * [[models.Familiarity.checked]] keeps, each in its language as the reader
    * stored it.
    */
  private def sighted
    (
      shown: List[Headword],
      storage: Browser.Storage,
    )
    : IO[Unit] = learner(storage).flatMap(reader =>
    familiarities(storage).saw(
      shown
        .take(Familiarity.most)
        .flatMap(headword =>
          (Familiarity.checked(headword), reader.learnt(headword.language))
            .mapN((headword, learnt) =>
              headword.copy(language = learnt.language),
            ),
        ),
    ),
  )

  /**
    * The card of the entry for `lookup`, for the reader, from their lexicon, as
    * kept on this device, or else as found, then kept (see
    * [[services.Lexicon.entry]]).
    */
  private def defining
    (lookup: Lookup, storage: Browser.Storage)
    : IO[Option[Card]] = (
    IO.fromOption(Lookup.checked(lookup))(Problem(
      "There is nothing to look up.",
    )),
    learner(storage),
  ).flatMapN((lookup, reader) =>
    IO.fromOption(reader.learnt(lookup.language))(Problem(
        "Only words of the languages you are learning are looked up.",
      ))
      .flatMap(learnt =>
        Lexicon
          .store
          .flatMap(Lexicon.entry(
            reader.native,
            lookup.copy(language = learnt.language),
            _,
            storage,
            Lexicon.fetch(_),
            [A] =>
              (use: (Model, ModelClient[IO]) => IO[A]) =>
                storage
                  .get(model)
                  .flatMap(chosen => client(storage).use(use(chosen, _))),
          ))
          .map(_.map(Card.of(learnt.language, _, lookup.meaning))),
      ),
  )

  /**
    * The reader, as they have chosen, while anything of the page is to be
    * marked, or else `None`, then again on each change of their choices or of
    * the page's address.
    */
  private def readers(storage: Browser.Storage): Stream[IO, Option[Learner]] =
    whenever(
      storage.changes(native).void :: storage.changes(fluent).void ::
        storage.changes(learning).void :: storage.changes(foreign).void ::
        websites(storage),
    )((learner(storage), ignores(storage)).mapN((learner, ignored) =>
      Some(learner).filter(_.active && !ignored),
    ))

  /**
    * Whether the page is left alone, then again on each change of which
    * websites have anything marked or of the page's address.
    */
  private def ignoring(storage: Browser.Storage): Stream[IO, Boolean] =
    whenever(websites(storage))(ignores(storage))

  /**
    * Each change of which websites have anything marked, or of the page's
    * address.
    */
  private def websites(storage: Browser.Storage): List[Stream[IO, Unit]] = List(
    storage.changes(scope).void,
    storage.changes(blocked).void,
    storage.changes(allowed).void,
    moves,
  )

  /**
    * Whether the page, at its address now, is left alone, as the reader's scope
    * and its list have it.
    */
  private def ignores(storage: Browser.Storage): IO[Boolean] =
    (storage.get(scope), page).flatMapN((scope, address) =>
      storage
        .get(listOf(scope))
        .map(sites => scope.leaves(Site.covered(sites, address))),
    )

  /** The websites that `scope` lists. */
  private[codeswitch] def listOf(scope: Scope): StorageKey[List[Site]] =
    scope match
      case Scope.Except => blocked
      case Scope.Only   => allowed

  /**
    * What `read` reads, at once, then again after each of `changes`, each time
    * that it is something else.
    */
  private def whenever[A : Eq]
    (changes: List[Stream[IO, Unit]])
    (read: IO[A])
    : Stream[IO, A] = (Stream.unit ++ Stream.emits(changes).parJoinUnbounded)
    .evalMap(_ => read)
    .changes

  /**
    * Each change of the page's address, as a single-page app moves to another
    * page without loading it, checked each second.
    */
  private def moves: Stream[IO, Unit] =
    (Stream.unit ++ Stream.awakeEvery[IO](1.second).void)
      .evalMap(_ => page)
      .changes
      .tail
      .void

  /** The address of the page, as its content script sees it. */
  private def page: IO[String] = IO(window.location.href)

  /** The reader, as they have chosen. */
  private def learner(storage: Browser.Storage): IO[Learner] = (
    storage.get(native),
    storage.get(fluent),
    storage.get(learning),
    storage.get(foreign),
  ).mapN(Learner.chosen)

  /**
    * Shares `problem` as the problem of the page's tab, which the popup may
    * dismiss, while in use.
    */
  private def offering
    (
      browser: TabBrowser,
      problem: SignallingRef[IO, String],
    )
    : Resource[IO, Unit] = browser.serve(dismiss)(_ => problem.set("")) >>
    browser.share(Codeswitch.problem)(problem)

  override def popup() = Some(Popup.page)

  /**
    * Takes out what earlier releases kept, which nothing reads now: the
    * language learnt, the reader's level in it and the two modes, and the sum
    * of familiarities kept before a sighting named its word's class, which
    * still counts the sightings that no longer read; and the dictionary's
    * entries kept before the lexicon, each named in its list, and the list.
    */
  private def retire(storage: Browser.Storage): IO[Unit] = retired.traverse_(
    storage.remove(_),
  ) >>
    storage
      .get(cached)
      .flatMap(_.traverse_(name =>
        storage.remove(StorageKey.local[Json](name, Json.Null)),
      )) >> storage.remove(cached)

  /**
    * The names of the dictionary's entries kept before the lexicon, the latest
    * used first (see [[retire]]).
    */
  private val cached = StorageKey.local[List[String]]("dictionary", Nil)

  /** The keys of what earlier releases kept (see [[retire]]). */
  private val retired: List[StorageKey[Json]] = List(
    "language",
    "level",
    "translating",
    "reading",
  ).map(StorageKey.sync[Json](_, Json.Null)) :+
    StorageKey.local[Json]("familiarityTotal", Json.Null)

  /**
    * What `message` gives for `request`, through `browser`, each entry of its
    * reply given to `each` as it arrives, before the rest, through `arrivals`.
    */
  private def calling[A, B]
    (
      browser: TabBrowser,
      message: Message[Streamed[A], List[B]],
      arrivals: Arrivals[B],
    )
    (request: A, each: Arrivals.Each[B])
    : IO[List[B]] = arrivals.asking(each)(id =>
    browser.runtime.call(message)(Streamed(id, request)),
  )

  /**
    * Sends each entry of the reply to the request `id`, by its index, as
    * `message` to the tab of `sender`, if any, whose page may have gone since.
    */
  private def sending[A]
    (
      tabs: Browser.Tabs,
      sender: Sender,
      message: Message.Tab[Arrival[A], Unit],
      id: String,
    )
    (index: Int, entry: A)
    : IO[Unit] = sender
    .tab
    .traverse_(tab =>
      tabs.call(message, tab.id)(Arrival(id, index, entry)).attempt.void,
    )

  /**
    * The translations of `words`, each into the language being learnt that it
    * asks for, as the reader has chosen it, each given to `each`, by its index,
    * as soon as the model has written it; but a word asking for one not being
    * learnt, as a page's content script may, is declined unasked, so that a
    * page cannot spend the reader's key on other languages. Once the reply has
    * ended, or failed, each word translated is kept as the word it was switched
    * into, which is counted as met as many times as the word stands for (see
    * [[services.FrequencyService]]).
    */
  private def translations
    (
      words: List[Word],
      storage: Browser.Storage,
    )
    (each: (Int, Translation) => IO[Unit])
    : IO[List[Translation]] = learner(storage).flatMap(reader =>
    val asked = words.map(word =>
      reader.learnt(word.into).map(learnt => word.copy(into = learnt.language)),
    )
    Ref[IO]
      .of(List.empty[Switch])
      .flatMap(switches =>
        translating(storage, asked)((index, translation) =>
          switches.update(
            asked(index).flatMap(Switch.of(_, translation)).toList ++ _,
          ) *> each(index, translation),
        ).guarantee(
          switches
            .get
            .flatMap(made =>
              counting(
                frequencies(storage).switched(reader.native, made.reverse),
              ),
            ),
        ),
      ),
  )

  /**
    * Counts how often words were met, as `count` does, only warning of a
    * failure, so that counting never fails the request it is of.
    */
  private def counting(count: IO[Unit]): IO[Unit] = count.handleErrorWith(
    error => IO(console.warn(s"Codeswitch: ${ error.getMessage }")),
  )

  /**
    * The translations of `words`, those that are none declined unasked, each of
    * the rest given to `each`, by its index among `words`, as soon as the model
    * has written it.
    */
  private def translating
    (
      storage: Browser.Storage,
      words: List[Option[Word]],
    )
    (each: (Int, Translation) => IO[Unit])
    : IO[List[Translation]] = words.flatten match
    case Nil   => IO.pure(words.map(_ => Translation.declined))
    case asked =>
      val places = words.zipWithIndex.collect { case (Some(_), place) => place }
      client(storage)
        .use(Translator.translate(_, asked)((index, translation) =>
          each(places(index), translation),
        ))
        .map(interleaved(words, _))

  /**
    * The translation of each of `words` in `translated`, in order, those of the
    * words that are none declined.
    */
  private def interleaved
    (
      words: List[Option[Word]],
      translated: List[Translation],
    )
    : List[Translation] = words
    .foldLeft((List.empty[Translation], translated)):
      case ((done, next :: rest), Some(_)) => (next :: done, rest)
      case ((done, rest), _) => (Translation.declined :: done, rest)
    ._1
    .reverse

  /**
    * The hints in `passages`, explained in the reader's own language, at their
    * level in the language that the passages are in, as stored, or none if it
    * is not being learnt, as a page's content script may ask, each passage's
    * given to `each`, by its index, as soon as the model has written them, once
    * each word hinted in it, if it is read for the first time, is counted as
    * met once more (see [[services.FrequencyService]]).
    */
  private def hintsFor
    (
      passages: Passages,
      storage: Browser.Storage,
    )
    (each: (Int, List[Hint]) => IO[Unit])
    : IO[List[List[Hint]]] = learner(storage).flatMap(reader =>
    reader
      .learnt(passages.learnt.language)
      .fold(IO.pure(passages.texts.map(_ => Nil)))(learnt =>
        client(storage).use(
          Translator.hint(_, learnt, reader.native, passages.texts)(
            (index, hints) =>
              counting(frequencies(storage).met(
                learnt.language,
                passages.met(List(index -> hints)),
              )) *> each(index, hints),
          ),
        ),
      ),
  )

  /**
    * `passages`, of foreign languages, put into the reader's own, each given to
    * `each`, by its index, as soon as the model has written it, or none of
    * them, unasked, if the reader would leave them, as a page's content script
    * may still ask.
    */
  private def renderings
    (
      passages: List[List[String]],
      storage: Browser.Storage,
    )
    (each: (Int, List[String]) => IO[Unit])
    : IO[List[List[String]]] = learner(storage).flatMap(reader =>
    if reader.foreign == Foreign.Translate then
      client(storage).use(Translator.render(_, reader.native, passages)(each))
    else IO.pure(passages.map(_ => Nil)),
  )

  /**
    * A client of the chosen model, which may stream its replies, with the
    * reader's key for its provider, charging what each request costs to what
    * has been spent.
    */
  private def client(storage: Browser.Storage): Resource[IO, ModelClient[IO]] =
    Resource
      .eval(
        for
          translator <- storage.get(model)
          key        <- storage.get(apiKeys).flatMap(keyFor(translator, _))
        yield Translator.config(translator, key),
      )
      .flatMap(ModelClient.resource[IO](_))
      .map(SpendingService(storage).charging)

  /** The key in `keys` for the provider of `translator`, if there is one. */
  private def keyFor
    (
      translator: Model,
      keys: Map[String, String],
    )
    : IO[String] = IO.fromOption(
    keys.get(translator.provider.id).map(_.trim).filter(_.nonEmpty),
  )(Problem(s"Add an API key from ${ translator.provider.displayName }."))

  /**
    * Translates `words` through `translate`, keeping in `problem` why the last
    * request failed, or nothing once one succeeds.
    */
  private[codeswitch] def noting
    (problem: Ref[IO, String])
    (translate: List[Word] => IO[List[Translation]])
    (words: List[Word])
    : IO[List[Translation]] =
    keeping(problem)(translate(words)).flatMap(recover(words, _))

  /**
    * The hints for `passages` through `hint`, keeping in `problem` why the last
    * request failed, or nothing once one succeeds. A failure marks nothing in
    * the passages, so that the rest of the page is still read and translated.
    */
  private[codeswitch] def hinting
    (problem: Ref[IO, String])
    (hint: Passages => IO[List[List[Hint]]])
    (passages: Passages)
    : IO[List[List[Hint]]] = keeping(problem)(hint(passages)).map(_.getOrElse(
    passages.texts.map(_ => Nil),
  ))

  /**
    * `passages` put into the reader's language through `render`, keeping in
    * `problem` why the last request failed, or nothing once one succeeds. A
    * failure translates none of them, so that the rest of the page is still
    * read and translated.
    */
  private[codeswitch] def rendering
    (problem: Ref[IO, String])
    (render: List[List[String]] => IO[List[List[String]]])
    (passages: List[List[String]])
    : IO[List[List[String]]] =
    keeping(problem)(render(passages)).map(_.getOrElse(passages.map(_ => Nil)))

  /**
    * How `request` went, keeping in `problem` why it failed, or nothing if it
    * succeeded.
    */
  private def keeping[A]
    (problem: Ref[IO, String])
    (request: IO[A])
    : IO[Either[Throwable, A]] = request
    .attempt
    .flatTap(outcome => problem.set(outcome.left.toOption.fold("")(reason)))

  /**
    * The translations of `words` in `outcome`, or its error. A reply that could
    * not be read, or matched to the words ([[services.Problem.ofBatch]]), is
    * kept as the problem, but each word is then left as it is, so that the
    * reader carries on reading.
    */
  private[codeswitch] def recover
    (
      words: List[Word],
      outcome: Either[Throwable, List[Translation]],
    )
    : IO[List[Translation]] = outcome match
    case Left(error) if Problem.ofBatch.exists(_.message == reason(error)) =>
      IO.pure(words.map(_ => Translation.declined))
    case other => IO.fromEither(other)

  /** Why translating failed, as the background explains it. */
  private[codeswitch] def reason(error: Throwable): String = error match
    case MessagingError(_, reason) => reason
    case other                     => other.getMessage

  /**
    * What `request` gives, reported on the toolbar button of `tab`, or failing
    * with why it could not be had, as the reader is told.
    */
  private def reporting[A]
    (
      action: Browser.Action,
      tab: Option[TabId],
    )
    (request: IO[A])
    : IO[A] = request
    .attempt
    .flatTap(report(action, tab, _))
    .rethrow
    .adaptError { case error => Problem(Translator.explain(error)) }

  /**
    * Marks the toolbar button of the `tab` whose request failed, whose content
    * script keeps why, or clears the mark once one succeeds there. With no tab,
    * the mark is on every tab's button.
    */
  private def report
    (
      action: Browser.Action,
      tab: Option[TabId],
      outcome: Either[Throwable, ?],
    )
    : IO[Unit] = mark(
    action,
    tab,
    if outcome.isLeft then "!" else "",
  )

  /** Shows `text` on the toolbar button of `tab`, or of every tab if none. */
  private[codeswitch] def mark
    (
      action: Browser.Action,
      tab: Option[TabId],
      text: String,
    )
    : IO[Unit] = tab.fold(
    action.setBadgeColour(problemColour) *> action.setBadge(text),
  )(tab =>
    action.setBadgeColour(problemColour, tab) *> action.setBadge(text, tab),
  )

  /** The colour of the mark that a problem puts on the toolbar button. */
  private val problemColour = "#dc2626"
