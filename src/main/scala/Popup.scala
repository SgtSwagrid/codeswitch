package com.alecdorrington.codeswitch

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.alecdorrington.omnibrowser.Browser
import org.scalajs.dom.*

/**
  * The popup of the toolbar button, in which the reader chooses the language to
  * learn and gives the API key to translate with.
  */
object Popup:

  /** Fills in the popup from `storage`, keeping each change there. */
  def show(storage: Browser.Storage): IO[Unit] =
    for
      language <- storage.get(Codeswitch.language)
      key      <- storage.get(Codeswitch.apiKey)
      problem  <- IO(element("p", "class" -> "problem")())
      _        <- IO(render(storage, language, key, problem))
      _        <- storage
        .values(Codeswitch.problem)
        .evalMap(text => IO(problem.textContent = text))
        .compile
        .drain
    yield ()

  private def render
    (
      storage: Browser.Storage,
      language: Option[Language],
      key: String,
      problem: Element,
    )
    : Unit =
    document.head.appendChild(element("style")(text(Style.popup)))
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
        "Language to learn",
        languages(storage, language),
      ),
      field("API key", apiKey(storage, key)),
      element("p", "class" -> "note")(text(
        "From Anthropic, OpenAI or Gemini. It is kept on this device, and sent only to its provider, with the words to translate.",
      )),
      problem,
    ).foreach(document.body.appendChild)

  private def languages
    (
      storage: Browser.Storage,
      chosen: Option[Language],
    )
    : HTMLSelectElement =
    val select = element("select")(
      (None +: Language.values.toSeq.map(Some(_))).map(option(chosen))*,
    ).asInstanceOf[HTMLSelectElement]
    select.onchange = _ =>
      storage
        .set(
          Codeswitch.language,
          Language.parse(select.value),
        )
        .unsafeRunAndForget()
    select

  private def option(chosen: Option[Language])(language: Option[Language]) =
    val option = element(
      "option",
      "value" -> language.fold("")(_.code),
    )(text(language.fold("None")(_.name))).asInstanceOf[HTMLOptionElement]
    option.selected = language == chosen
    option

  private def apiKey(storage: Browser.Storage, key: String): HTMLInputElement =
    val input = element(
      "input",
      "type"         -> "password",
      "autocomplete" -> "off",
      "spellcheck"   -> "false",
    )().asInstanceOf[HTMLInputElement]
    input.value = key
    // On each change, as the popup may close before the field loses focus.
    input.oninput = _ =>
      (storage.set(Codeswitch.apiKey, input.value.trim) *>
        storage.set(Codeswitch.problem, "")).unsafeRunAndForget()
    input

  private def field(label: String, control: Element): Element =
    element("label")(element("span")(text(label)), control)

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

  private def text(content: String): Text = document.createTextNode(content)
