package com.alecdorrington.codeswitch

import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.alecdorrington.codeswitch.Translator.model
import com.alecdorrington.iris.{LlmClient, LlmModel, LlmProvider}
import com.alecdorrington.megalingual.Language
import com.alecdorrington.omnibrowser.*
import com.alecdorrington.omnibrowser.Permission.Storage
import fs2.Stream
import fs2.concurrent.SignallingRef
import io.circe.{Codec, Decoder, Encoder, Json}

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
  // read, go to the provider, and the words looked up to Kaikki.
  override val dataCollection = DataCollection(Set(PersonalData.WebsiteContent))

  // So that the background may ask each provider, and Kaikki for Wiktionary's
  // entries, whether or not each allows requests from the extension's origin.
  // The pages that the content script runs in already cover Kaikki's, so its
  // host asks the reader for nothing more when they update.
  override def hosts = Wiktionary.host +:
    LlmProvider.values.toSeq.map(provider => s"${ provider.origin }/*")

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

  /**
    * The reader's own language, in which words are explained, if they have
    * chosen it, or else the browser's first that is not being learnt (see
    * [[Learner.own]]).
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
    * The share of the words of each page to translate, from `0` up to
    * [[Words.maxIntensity]].
    */
  val intensity = StorageKey.sync[Double]("intensity", Words.defaultIntensity)

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
    * Translates words, in their contexts, each into a language being learnt, or
    * fails with why it could not.
    */
  val translate = Message[List[Word], List[Translation]]("translate")

  /**
    * Finds the words of passages in a language being learnt that are above the
    * reader's level, in each, or fails with why it could not.
    */
  val hints = Message[Passages, List[List[Hint]]]("hints")

  /**
    * The card of a word that the reader looks up, if the dictionary has an
    * entry for it, or fails with why it could not be had.
    */
  val define = Message[Lookup, Option[Card]]("define")

  override def background() = Background(
    serve(translate)((words, sender, browser) =>
      reporting(browser.action, sender.tab.map(_.id))(
        translations(words, browser.storage),
      ),
    ),
    serve(hints)((passages, sender, browser) =>
      reporting(browser.action, sender.tab.map(_.id))(
        hintsFor(passages, browser.storage),
      ),
    ),
    serve(define)((lookup, browser) =>
      defining(lookup, browser.storage).adaptError { case error =>
        Problem(Translator.explain(error))
      },
    ),
  )

  override def contentScripts() = Seq(
    ContentScript("http://*/*", "https://*/*")(browser =>
      SignallingRef[IO]
        .of("")
        .flatMap(problem =>
          (Tips.shown(
            rating => browser.run(rate(browser.storage, rating)),
            lookup =>
              browser
                .runtime
                .call(define)(lookup)
                .attempt
                .map(_.left.map(reason)),
            browser.run,
          ) <* offering(browser, problem)).use(tips =>
            Reader.read(
              learners(browser.storage).map(Some(_).filter(_.active)).changes,
              browser.storage.values(intensity),
              Asking(
                noting(problem)(browser.runtime.call(translate)),
                hinting(problem)(browser.runtime.call(hints)),
                browser.languages.detect,
                familiarities(browser.storage),
              ),
              tips,
            ),
          ),
        ),
    ).withCss("content.css"),
  )

  /**
    * Keeps the reader's `rating` of a word of a language being learnt, on this
    * device (see [[Familiarity.key]]).
    */
  private def rate(storage: Browser.Storage, rating: Rating): IO[Unit] = storage
    .update(Familiarity.key(rating.language, rating.word))(familiarity =>
      Some(Familiarity.rate(familiarity, rating)),
    )
    .void

  /**
    * How well the reader knows each of `words` of `learnt` that they have
    * rated.
    */
  private def familiarities
    (storage: Browser.Storage)
    (learnt: Language, words: List[String])
    : IO[Map[String, Double]] = words
    .map(_.trim)
    .distinct
    .parTraverse(word =>
      storage.get(Familiarity.key(learnt, word)).map(_.map(word -> _)),
    )
    .map(_.flatten.toMap)

  /**
    * The card of the dictionary's entry for `lookup`, for the reader, as kept
    * on this device, or else as found, then kept (see [[Dictionary.entry]]).
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
        Dictionary
          .entry(
            reader.native,
            lookup.copy(language = learnt.language),
            storage,
            Dictionary.fetch(_),
            [A] => (use: LlmClient[IO] => IO[A]) => client(storage).use(use),
          )
          .map(_.map(Card.of(learnt.language, _))),
      ),
  )

  /**
    * The reader, as they have chosen, then again on each change of their
    * choices.
    */
  private def learners(storage: Browser.Storage): Stream[IO, Learner] =
    (Stream.unit ++ Stream(
      storage.changes(native).void,
      storage.changes(fluent).void,
      storage.changes(learning).void,
    ).parJoinUnbounded).evalMap(_ => learner(storage))

  /** The reader, as they have chosen. */
  private def learner(storage: Browser.Storage): IO[Learner] =
    (storage.get(native), storage.get(fluent), storage.get(learning)).mapN(
      Learner.chosen,
    )

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
    * The translations of `words`, each into the language being learnt that it
    * asks for, as the reader has chosen its variety; but a word asking for one
    * not being learnt, as a page's content script may, is declined unasked, so
    * that a page cannot spend the reader's key on other languages.
    */
  private def translations
    (
      words: List[Word],
      storage: Browser.Storage,
    )
    : IO[List[Translation]] = learner(storage).flatMap(reader =>
    translating(
      storage,
      words.map(word =>
        reader
          .learnt(word.into)
          .map(learnt => word.copy(into = learnt.language)),
      ),
    ),
  )

  /** The translations of `words`, those that are none declined unasked. */
  private def translating
    (
      storage: Browser.Storage,
      words: List[Option[Word]],
    )
    : IO[List[Translation]] = words.flatten match
    case Nil   => IO.pure(words.map(_ => Translation.declined))
    case asked => client(storage)
        .use(Translator.translate(_, asked))
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
    * is not being learnt, as a page's content script may ask.
    */
  private def hintsFor
    (
      passages: Passages,
      storage: Browser.Storage,
    )
    : IO[List[List[Hint]]] = learner(storage).flatMap(reader =>
    reader
      .learnt(passages.learnt.language)
      .fold(IO.pure(passages.texts.map(_ => Nil)))(learnt =>
        client(storage).use(
          Translator.hint(_, learnt, reader.native, passages.texts),
        ),
      ),
  )

  /** A client of the chosen model, with the reader's key for its provider. */
  private def client(storage: Browser.Storage): Resource[IO, LlmClient[IO]] =
    Resource
      .eval(
        for
          translator <- storage.get(model)
          key        <- storage.get(apiKeys).flatMap(keyFor(translator, _))
        yield Translator.config(translator, key),
      )
      .flatMap(LlmClient.resource[IO](_))

  /** The key in `keys` for the provider of `translator`, if there is one. */
  private def keyFor
    (
      translator: LlmModel,
      keys: Map[String, String],
    )
    : IO[String] = IO.fromOption(
    keys.get(translator.provider.key).map(_.trim).filter(_.nonEmpty),
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
    * not be read, or matched to the words ([[Problem.ofBatch]]), is kept as the
    * problem, but each word is then left as it is, so that the reader carries
    * on reading.
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
  private def reason(error: Throwable): String = error match
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
