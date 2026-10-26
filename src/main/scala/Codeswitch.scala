package com.alecdorrington.codeswitch

import cats.effect.IO
import cats.syntax.all.*
import com.alecdorrington.codeswitch.Translator.given
import com.alecdorrington.iris.{LlmClient, LlmModel, LlmProvider}
import com.alecdorrington.omnibrowser.*
import com.alecdorrington.omnibrowser.Permission.Storage

/**
  * Teaches you a language by translating a few words of every page you read.
  * Each word of a page has a chance of [[Words.share]] of being shown in the
  * language that the reader is learning, highlighted, with the original word
  * shown above it on hovering over it.
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
  override val hosts = LlmProvider
    .values
    .toSeq
    .map(provider => s"${ provider.origin }/*")

  /** The language that the reader is learning, if any. */
  val language = StorageKey.sync[Option[Language]]("language", None)

  /** The model that translates, which names the provider that it is asked of. */
  val model = StorageKey.sync[LlmModel]("model", Translator.defaultModel)

  /** The reader's API keys, by the key of the provider each is for. */
  val apiKeys = StorageKey.local[Map[String, String]]("apiKeys", Map.empty)

  /** Why the last translation failed, or nothing if it did not. */
  val problem = StorageKey.local[String]("problem", "")

  /** Translates words, in their contexts, into the language being learnt. */
  val translate = Endpoint[List[Word], List[Option[String]]]("translate")

  override val background = Background(serve(translate)((words, browser) =>
    translations(words, browser.storage)
      .attempt
      .flatTap(report(browser, _))
      .rethrow,
  ))

  override val contentScripts = Seq(
    ContentScript("http://*/*", "https://*/*")(browser =>
      Reader.read(
        browser.storage.values(language),
        browser.runtime.call(translate),
      ),
    ).withCss("content.css"),
  )

  override val popup = Some(Popup.page)

  private def translations
    (
      words: List[Word],
      storage: Browser.Storage,
    )
    : IO[List[Option[String]]] =
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
    * and the reason in the popup, or clears both once it succeeds again.
    */
  private def report
    (
      browser: Browser,
      outcome: Either[Throwable, ?],
    )
    : IO[Unit] = outcome match
    case Left(error) =>
      browser.storage.set(problem, Translator.explain(error)) *>
        browser.action.setBadgeColour("#dc2626") *> browser.action.setBadge("!")
    case Right(_) => browser
        .storage
        .get(problem)
        .flatMap(reported =>
          (browser.storage.set(problem, "") *> browser.action.setBadge(""))
            .whenA(reported.nonEmpty),
        )
