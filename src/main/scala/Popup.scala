package com.alecdorrington.codeswitch

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.alecdorrington.iris.{LlmModel, LlmProvider}
import com.alecdorrington.omnibrowser.{Browser, Granted}
import com.alecdorrington.omnibrowser.Permission.Storage
import org.scalajs.dom.*

/**
  * The popup of the toolbar button, in which the reader chooses the language to
  * learn and the model to translate with, and gives an API key for its
  * provider.
  */
object Popup:

  /**
    * Fills in the popup from the browser's storage, keeping each change there,
    * and clearing the problem shown, which the change may have solved.
    */
  def show(browser: Browser)(using Granted[Storage]): IO[Unit] =
    val storage = browser.storage
    for
      language <- storage.get(Codeswitch.language)
      model    <- storage.get(Codeswitch.model)
      keys     <- storage.get(Codeswitch.apiKeys)
      problem  <- IO(element("p", "class" -> "problem")())
      _        <- IO(render(
        storage,
        dismiss(browser),
        language,
        model,
        keys,
        problem,
      ))
      _ <- storage
        .values(Codeswitch.problem)
        .evalMap(text => IO(problem.textContent = text))
        .compile
        .drain
    yield ()

  /** Clears the problem shown, and the mark on the toolbar button. */
  private def dismiss(browser: Browser)(using Granted[Storage]): IO[Unit] =
    browser.storage.set(Codeswitch.problem, "") *> browser.action.setBadge("")

  private def render
    (
      storage: Browser.Storage,
      dismiss: IO[Unit],
      language: Option[Language],
      model: LlmModel,
      keys: Map[String, String],
      problem: Element,
    )
    : Unit =
    document.head.appendChild(element("style")(text(Style.popup)))
    val key = KeyField(storage, dismiss, model.provider, keys)
    Seq(
      element("header")(
        element(
          "img",
          "src" -> "icons/icon-32.png",
          "alt" -> "",
        )(),
        element("h1")(text("Codeswitch")),
      ),
      field(
        text("Language to learn"),
        languages(storage, language),
      ),
      field(
        text("Model"),
        models(storage, dismiss, model, key),
      ),
      field(key.label, key.input),
      element("p", "class" -> "note")(text(
        "The key is kept on this device, and sent only to its provider, with the words to translate.",
      )),
      problem,
    ).foreach(document.body.appendChild)

  private def languages
    (
      storage: Browser.Storage,
      chosen: Option[Language],
    )
    : HTMLSelectElement =
    select((None +: Language.values.toSeq.map(Some(_))).map(language =>
      option(
        language.fold("")(_.code),
        language.fold("None")(_.name),
        language == chosen,
      ),
    ))(code =>
      storage.set(
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
      storage: Browser.Storage,
      dismiss: IO[Unit],
      chosen: LlmModel,
      key: KeyField,
    )
    : HTMLSelectElement = select(
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
      .flatTap(storage.set(Codeswitch.model, _))
      .flatMap(model => key.show(model.provider) *> dismiss),
  )

  private def label(model: LlmModel): String =
    if model == Translator.defaultModel then s"${ model.id } (quick)"
    else model.id

  private def select
    (options: Seq[Element])
    (choose: String => IO[Unit])
    : HTMLSelectElement =
    val select = element("select")(options*).asInstanceOf[HTMLSelectElement]
    select.onchange = _ => choose(select.value).unsafeRunAndForget()
    select

  private def option
    (
      value: String,
      label: String,
      selected: Boolean,
    ) =
    val option = element("option", "value" -> value)(
      text(label),
    ).asInstanceOf[HTMLOptionElement]
    option.selected = selected
    option

  private def field(label: Node, control: Element): Element =
    element("label")(element("span")(label), control)

  private[codeswitch] def element
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

  private[codeswitch] def text(content: String): Text =
    document.createTextNode(content)

/**
  * The field for the API key of one provider at a time, which keeps each
  * provider's key apart in `storage`, starting with `provider`'s of `keys`, and
  * runs `dismiss` on each change.
  */
private final class KeyField
  (
    storage: Browser.Storage,
    dismiss: IO[Unit],
    provider: LlmProvider,
    keys: Map[String, String],
  ):

  private var current = provider

  /** The label naming the provider whose key is shown. */
  val label: Text = Popup.text(name(provider))

  /** The key of the provider whose key is shown. */
  val input: HTMLInputElement = Popup
    .element(
      "input",
      "type"         -> "password",
      "autocomplete" -> "off",
      "spellcheck"   -> "false",
    )()
    .asInstanceOf[HTMLInputElement]

  input.value = keys.getOrElse(provider.key, "")

  // On each change, as the popup may close before the field loses focus.
  input.oninput = _ => save(current, input.value.trim).unsafeRunAndForget()

  /** Shows the key of `provider` in place of the one shown. */
  def show(provider: LlmProvider): IO[Unit] = storage
    .get(Codeswitch.apiKeys)
    .flatMap(keys =>
      IO:
        current = provider
        label.textContent = name(provider)
        input.value = keys.getOrElse(provider.key, ""),
    )

  private def save(provider: LlmProvider, key: String): IO[Unit] = storage
    .update(Codeswitch.apiKeys)(_.updated(provider.key, key)) *> dismiss

  private def name(provider: LlmProvider): String =
    s"${ provider.displayName } API key"
