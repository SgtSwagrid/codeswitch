package com.alecdorrington.codeswitch

import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.alecdorrington.codeswitch.Translator.model
import com.alecdorrington.iris.{LlmClient, LlmModel, LlmProvider}
import com.alecdorrington.omnibrowser.*
import com.alecdorrington.omnibrowser.Permission.Storage
import fs2.Stream
import fs2.concurrent.SignallingRef

/**
  * Teaches you a language by translating a few words of every page you read.
  * Each word of a page in another language has a chance, the [[intensity]] that
  * the reader chooses, of being shown in the language that the reader is
  * learning, highlighted, with the original word shown above it on hovering
  * over it. In a page in that language, the words a level above the reader's
  * are highlighted, with their meanings shown above them, and harder words are
  * shown as their meanings, in the reader's own language.
  */
object Codeswitch extends BrowserExtension[Storage]:

  override val name = "Codeswitch"

  override val description =
    "Teaches you a language by translating a few words of every page you read."

  override val icons = Seq(16, 32, 48, 128)
    .map(size => size -> s"icons/icon-$size.png")
    .toMap

  // The words to translate, with the text around them, and the passages to
  // read, go to the provider.
  override val dataCollection = DataCollection(Set(PersonalData.WebsiteContent))

  // So that the background may ask each provider, whether or not its API
  // allows requests from the extension's origin.
  override def hosts = LlmProvider
    .values
    .toSeq
    .map(provider => s"${ provider.origin }/*")

  // Every page loads what this object's own values reach, so the key of the
  // model, whose codec and default reach Iris's models, is on `Translator`.

  /** The language that the reader is learning, if any. */
  val language = StorageKey.sync[Option[Language]]("language", None)

  /**
    * Whether a share of the words of pages in other languages are shown in the
    * language being learnt.
    */
  val translating = StorageKey.sync[Boolean]("translating", true)

  /**
    * The share of the words of each page to translate, from `0` up to
    * [[Words.maxIntensity]].
    */
  val intensity = StorageKey.sync[Double]("intensity", Words.defaultIntensity)

  /** Whether the words of pages in the language being learnt are marked. */
  val reading = StorageKey.sync[Boolean]("reading", true)

  /**
    * The reader's own language, in which to explain the one being learnt, if
    * they have chosen it, or else the browser's first that is not being learnt
    * (see [[Language.own]]).
    */
  val native = StorageKey.sync[Option[Language]]("native", None)

  /** How well the reader knows the language being learnt. */
  val level = StorageKey.sync[Level]("level", Level.B1)

  /**
    * How well the reader knows each word above their level that they have
    * rated, from `0` to `1`, by its dictionary form (see [[Familiarity]]). It
    * is kept on this device, as sync storage holds only 8 KB in each value.
    */
  val familiarity =
    StorageKey.local[Map[String, Double]]("familiarity", Map.empty)

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
    * Translates words, in their contexts, into the language being learnt, or
    * fails with why it could not.
    */
  val translate = Message[List[Word], List[Translation]]("translate")

  /**
    * Finds the words of passages in the language being learnt that are above
    * the reader's level, in each, or fails with why it could not.
    */
  val hints = Message[List[String], List[List[Hint]]]("hints")

  override def background() = Background(
    serve(translate)((words, sender, browser) =>
      reporting(browser.action, sender.tab.map(_.id))(
        translations(words, browser.storage),
      ),
    ),
    serve(hints)((passages, sender, browser) =>
      reporting(browser.action, sender.tab.map(_.id))(hintsFor(
        passages,
        browser.storage,
      )),
    ),
  )

  override def contentScripts() = Seq(
    ContentScript("http://*/*", "https://*/*")(browser =>
      SignallingRef[IO]
        .of("")
        .flatMap(problem =>
          (Tooltip.shown(rating =>
            browser.run(
              browser
                .storage
                .update(familiarity)(Familiarity.rate(
                  _,
                  rating.word,
                  rating.kind,
                  rating.answer,
                ))
                .void,
            ),
          ) >> offering(browser, problem)).surround(Reader.read(
            learners(browser.storage).map(_.filter(_.active)).changes,
            browser.storage.values(intensity),
            Asking(
              noting(problem)(browser.runtime.call(translate)),
              hinting(problem)(browser.runtime.call(hints)),
              browser.languages.detect,
              browser.storage.get(familiarity),
            ),
          )),
        ),
    ).withCss("content.css"),
  )

  /**
    * The reader, as they have chosen, if they have chosen a language to learn,
    * then again on each change of their choices.
    */
  private def learners(storage: Browser.Storage): Stream[IO, Option[Learner]] =
    (Stream.unit ++ Stream(
      storage.changes(language).void,
      storage.changes(native).void,
      storage.changes(level).void,
      storage.changes(translating).void,
      storage.changes(reading).void,
    ).parJoinUnbounded).evalMap(_ => learner(storage))

  /** The reader, as they have chosen, if they have chosen a language to learn. */
  private def learner(storage: Browser.Storage): IO[Option[Learner]] = (
    storage.get(language),
    storage.get(native),
    storage.get(level),
    storage.get(translating),
    storage.get(reading),
  ).mapN(
    (
      language,
      native,
      level,
      translating,
      reading,
    ) =>
      language.map(learnt =>
        Learner(
          learnt,
          native.getOrElse(Language.own(Language.preferred, Some(learnt))),
          level,
          translating,
          reading,
        ),
      ),
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

  private def translations
    (
      words: List[Word],
      storage: Browser.Storage,
    )
    : IO[List[Translation]] = asking(storage)((client, reader) =>
    Translator.translate(client, reader.language, words),
  )

  private def hintsFor
    (
      passages: List[String],
      storage: Browser.Storage,
    )
    : IO[List[List[Hint]]] = asking(storage)(Translator.hint(_, _, passages))

  /**
    * What `ask` gives, of the chosen model, with the reader's key for its
    * provider, for the reader, who must have chosen a language to learn.
    */
  private def asking[A]
    (storage: Browser.Storage)
    (ask: (LlmClient[IO], Learner) => IO[A])
    : IO[A] =
    for
      reader <- learner(storage).flatMap(IO.fromOption(_)(Problem(
        "Choose a language to learn.",
      )))
      translator <- storage.get(model)
      key        <- storage.get(apiKeys).flatMap(keyFor(translator, _))
      answer     <- LlmClient
        .resource[IO](Translator.config(translator, key))
        .use(ask(_, reader))
    yield answer

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
    (hint: List[String] => IO[List[List[Hint]]])
    (passages: List[String])
    : IO[List[List[Hint]]] =
    keeping(problem)(hint(passages)).map(_.getOrElse(passages.map(_ => Nil)))

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
    * The translations of `words` in `outcome`, or its error. Translations that
    * could not be matched to the words are kept as the problem, but each word
    * is then left as it is, so that the reader carries on reading.
    */
  private[codeswitch] def recover
    (
      words: List[Word],
      outcome: Either[Throwable, List[Translation]],
    )
    : IO[List[Translation]] = outcome match
    case Left(error) if reason(error) == Problem.unmatched.message =>
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
