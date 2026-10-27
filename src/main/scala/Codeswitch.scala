package com.alecdorrington.codeswitch

import cats.effect.{IO, Ref, Resource}
import cats.syntax.all.*
import com.alecdorrington.codeswitch.Translator.model
import com.alecdorrington.iris.{LlmClient, LlmModel, LlmProvider}
import com.alecdorrington.omnibrowser.*
import com.alecdorrington.omnibrowser.Permission.Storage
import fs2.concurrent.SignallingRef

/**
  * Teaches you a language by translating a few words of every page you read.
  * Each word of a page has a chance, the [[intensity]] that the reader chooses,
  * of being shown in the language that the reader is learning, highlighted,
  * with the original word shown above it on hovering over it.
  */
object Codeswitch extends BrowserExtension[Storage]:

  override val name = "Codeswitch"

  override val description =
    "Teaches you a language by translating a few words of every page you read."

  override val icons = Seq(16, 32, 48, 128)
    .map(size => size -> s"icons/icon-$size.png")
    .toMap

  // The words to translate, and the text around them, go to the provider.
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
    * The share of the words of each page to translate, from `0` up to
    * [[Words.maxIntensity]].
    */
  val intensity = StorageKey.sync[Double]("intensity", Words.defaultIntensity)

  /** The reader's API keys, by the key of the provider each is for. */
  val apiKeys = StorageKey.local[Map[String, String]]("apiKeys", Map.empty)

  /**
    * Why the last translation of a tab's page failed, or nothing if it did not,
    * which the page's content script shares, and the popup shows for its tab.
    * It lasts only as long as the page does.
    */
  val problem = Shared.tab[String]("problem", "")

  /** Forgets why a page's last translation failed, as the reader has acted. */
  val dismiss = Endpoint.tab[Unit, Unit]("dismiss")

  /**
    * Translates words, in their contexts, into the language being learnt, or
    * fails with why it could not.
    */
  val translate = Endpoint[List[Word], List[Translation]]("translate")

  override def background() = Background(
    serve(translate)((words, sender, browser) =>
      translations(words, browser.storage)
        .attempt
        .flatTap(report(browser.action, sender.tab.map(_.id), _))
        .rethrow
        .adaptError { case error => Problem(Translator.explain(error)) },
    ),
  )

  override def contentScripts() = Seq(
    ContentScript("http://*/*", "https://*/*")(browser =>
      SignallingRef[IO]
        .of("")
        .flatMap(problem =>
          (Tooltip.shown >> offering(browser, problem)).surround(Reader.read(
            browser.storage.values(language),
            browser.storage.values(intensity),
            noting(problem)(browser.runtime.call(translate)),
          )),
        ),
    ).withCss("content.css"),
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
    : IO[List[Translation]] =
    for
      chosen <- storage
        .get(language)
        .flatMap(IO.fromOption(_)(Problem("Choose a language to learn.")))
      translator   <- storage.get(model)
      key          <- storage.get(apiKeys).flatMap(keyFor(translator, _))
      translations <- LlmClient
        .resource[IO](Translator.config(translator, key))
        .use(Translator.translate(_, chosen, words))
    yield translations

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
    * translation failed, or nothing once one succeeds.
    */
  private[codeswitch] def noting
    (problem: Ref[IO, String])
    (translate: List[Word] => IO[List[Translation]])
    (words: List[Word])
    : IO[List[Translation]] = translate(words)
    .attempt
    .flatTap(outcome => problem.set(outcome.left.toOption.fold("")(reason)))
    .flatMap(recover(words, _))

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
    * Marks the toolbar button of the `tab` whose translation failed, whose
    * content script keeps why, or clears the mark once one succeeds there. With
    * no tab, the mark is on every tab's button.
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
