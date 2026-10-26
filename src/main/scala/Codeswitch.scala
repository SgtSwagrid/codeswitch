package com.alecdorrington.codeswitch

import cats.effect.IO
import cats.syntax.all.*
import com.alecdorrington.codeswitch.Translator.model
import com.alecdorrington.iris.{LlmClient, LlmModel, LlmProvider}
import com.alecdorrington.omnibrowser.*
import com.alecdorrington.omnibrowser.Permission.Storage

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
    * Why the last translation failed, or nothing if it did not, which the popup
    * shows. It lasts only as long as the browser stays open.
    */
  val problem = StorageKey.session[String]("problem", "")

  /** Translates words, in their contexts, into the language being learnt. */
  val translate = Endpoint[List[Word], List[Translation]]("translate")

  override def background() = Background(
    serve(translate)((words, sender, browser) =>
      translations(words, browser.storage)
        .attempt
        .flatTap(report(browser, sender.tab.map(_.id), _))
        .rethrow,
    ),
  )

  override def contentScripts() = Seq(
    ContentScript("http://*/*", "https://*/*")(browser =>
      Tooltip
        .shown
        .surround(Reader.read(
          browser.storage.values(language),
          browser.storage.values(intensity),
          browser.runtime.call(translate),
        )),
    ).withCss("content.css"),
  )

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
    * Tells the reader why translating failed, with a mark on the toolbar button
    * of the `tab` it failed in and the reason in the popup, or clears both once
    * it succeeds there. With no tab, the mark is on every tab's button.
    */
  private def report
    (
      browser: Browser,
      tab: Option[TabId],
      outcome: Either[Throwable, ?],
    )
    : IO[Unit] = outcome match
    case Left(error) =>
      browser.storage.set(problem, Translator.explain(error)) *>
        mark(browser.action, tab, "!")
    case Right(_) => mark(browser.action, tab, "") *>
        browser
          .storage
          .get(problem)
          .flatMap(reported =>
            browser.storage.set(problem, "").whenA(reported.nonEmpty),
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
