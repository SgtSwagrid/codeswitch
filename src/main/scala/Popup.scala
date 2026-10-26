package com.alecdorrington.codeswitch

import cats.effect.IO
import cats.syntax.all.*
import com.alecdorrington.iris.{LlmModel, LlmProvider}
import com.alecdorrington.omnibrowser.{Browser, Granted, Page}
import com.alecdorrington.omnibrowser.Permission.Storage
import org.scalajs.dom.*

/**
  * The popup of the toolbar button, in which the reader chooses the language to
  * learn and the model to translate with, and gives an API key for its
  * provider.
  */
object Popup:

  /** The popup, from its HTML and `popup.css`, which [[show]] fills in. */
  def page(using Granted[Storage]): Page = Page(show(_))
    .withBody(body)
    .withStylesheets("popup.css")

  private val body = """<header>
      |  <img src="icons/icon-32.png" alt="">
      |  <h1>Codeswitch</h1>
      |</header>
      |<label><span>Language to learn</span><select id="language"></select></label>
      |<label><span>Model</span><select id="model"></select></label>
      |<label><span id="key-name"></span><input id="key" type="password" autocomplete="off" spellcheck="false"></label>
      |<p class="note">The key is kept on this device, and sent only to its provider, with the words to translate.</p>
      |<p class="problem" id="problem"></p>""".stripMargin

  /**
    * Fills in the popup from the browser's storage, keeping each change there,
    * and clearing the problem shown, which the change may have solved.
    */
  def show(browser: Browser)(using Granted[Storage]): IO[Unit] =
    val storage = browser.storage
    for
      language <- storage.get(Codeswitch.language)
      model    <- storage.get(Translator.model)
      keys     <- storage.get(Codeswitch.apiKeys)
      key      <- IO(KeyField(browser, model.provider, keys))
      _        <- IO(languages(browser, language))
      _        <- IO(models(browser, model, key))
      _        <- storage
        .values(Codeswitch.problem)
        .evalMap(text => IO(find[Element]("problem").textContent = text))
        .compile
        .drain
    yield ()

  /**
    * Clears the problem shown, and the mark on the toolbar button, both of
    * every tab and of the tab that the popup was opened over.
    */
  private[codeswitch] def dismiss
    (browser: Browser)
    (using Granted[Storage])
    : IO[Unit] = browser.storage.set(Codeswitch.problem, "") *>
    Codeswitch.mark(browser.action, None, "") *>
    browser
      .tabs
      .active
      .flatMap(_.traverse_(tab =>
        Codeswitch.mark(browser.action, Some(tab.id), ""),
      ))

  private def languages
    (
      browser: Browser,
      chosen: Option[Language],
    )
    (using Granted[Storage])
    : Unit = fill(
    browser,
    "language",
    (None +: Language.values.toSeq.map(Some(_))).map(language =>
      option(
        language.fold("")(_.code),
        language.fold("None")(_.name),
        language == chosen,
      ),
    ),
  )(code =>
    browser
      .storage
      .set(
        Codeswitch.language,
        Language.parse(code),
      ),
  )

  /**
    * The models, by provider, each named as its provider names it. Choosing one
    * of another provider shows that provider's key.
    */
  private def models
    (
      browser: Browser,
      chosen: LlmModel,
      key: KeyField,
    )
    (using Granted[Storage])
    : Unit = fill(
    browser,
    "model",
    Translator
      .models
      .map((provider, models) =>
        element(
          "optgroup",
          "label" -> provider.displayName,
        )(
          models.map(model =>
            option(model.name, label(model), model == chosen),
          )*,
        ),
      ),
  )(name =>
    IO.fromOption(LlmModel.parse(name))(Problem(s"No model is named $name."))
      .flatTap(browser.storage.set(Translator.model, _))
      .flatMap(model => key.show(model.provider) *> dismiss(browser)),
  )

  private def label(model: LlmModel): String =
    if model == Translator.defaultModel then s"${ model.id } (quick)"
    else model.id

  /** Fills the menu `id` with `options`, and gives `choose` each choice. */
  private def fill
    (
      browser: Browser,
      id: String,
      options: Seq[Element],
    )
    (choose: String => IO[Unit])
    : Unit =
    val select = find[HTMLSelectElement](id)
    options.foreach(select.appendChild)
    select.onchange = _ => browser.run(choose(select.value))

  private def option
    (
      value: String,
      label: String,
      selected: Boolean,
    ) =
    val option =
      element("option", "value" -> value)(document.createTextNode(label))
        .asInstanceOf[HTMLOptionElement]
    option.selected = selected
    option

  private def element
    (
      tag: String,
      attributes: (String, String)*,
    )
    (children: Node*)
    : Element =
    val element = document.createElement(tag)
    attributes.foreach(element.setAttribute)
    children.foreach(element.appendChild)
    element

  /** The element of the popup's HTML whose ID is `id`. */
  private[codeswitch] def find[E <: Element](id: String): E = document
    .getElementById(id)
    .asInstanceOf[E]

/**
  * The field of the popup for the API key of one provider at a time, which
  * keeps each provider's key apart, starting with `provider`'s of `keys`.
  */
private final class KeyField
  (
    browser: Browser,
    provider: LlmProvider,
    keys: Map[String, String],
  )
  (using Granted[Storage]):

  private var current = provider

  private val name  = Popup.find[Element]("key-name")
  private val input = Popup.find[HTMLInputElement]("key")

  name.textContent = title(provider)
  input.value = keys.getOrElse(provider.key, "")

  // On each change, as the popup may close before the field loses focus.
  input.oninput = _ => browser.run(save(current, input.value.trim))

  /** Shows the key of `provider` in place of the one shown. */
  def show(provider: LlmProvider): IO[Unit] = browser
    .storage
    .get(Codeswitch.apiKeys)
    .flatMap(keys =>
      IO:
        current = provider
        name.textContent = title(provider)
        input.value = keys.getOrElse(provider.key, ""),
    )

  private def save(provider: LlmProvider, key: String): IO[Unit] = browser
    .storage
    .update(Codeswitch.apiKeys)(_.updated(provider.key, key)) *>
    Popup.dismiss(browser)

  private def title(provider: LlmProvider): String =
    s"${ provider.displayName } API key"
