package com.alecdorrington.codeswitch

import cats.effect.{IO, Resource}
import com.alecdorrington.annotatedtextformat.dom.{Hovering, Tooltip}
import org.scalajs.dom.*

/**
  * The tooltips of the page: of the translation under the pointer, its original
  * text, and of the hint, its meaning, or the original of a gloss, with a
  * button for each [[Answer]] to whether the reader knew its word. Each is
  * shown above the line under the pointer, or below it where there is no room
  * above, in the page's top layer, where nothing in the page can clip or cover
  * it. A hint's stays open while the pointer goes from the word to it, and each
  * hint takes the focus in its turn, as by Tab, showing its tooltip, whose
  * buttons Tab moves on to. A tap shows either, as hovering does.
  *
  * @param translations
  *   What covers the translations.
  * @param hints
  *   What covers the hints.
  */
final class Tips private (
  val translations: Hovering[Translated],
  val hints: Hovering[Marking],
):

  /** Covers no translation or hint, closing either tooltip. */
  def clear(): Unit =
    translations.clear()
    hints.clear()

  /** Takes both tooltips out of the page, and stops following the pointer. */
  def stop(): Unit = List(translations, hints).foreach(hovering =>
    hovering.stop()
    hovering.tooltip.remove(),
  )

object Tips:

  /** The class of the row of buttons in a hint's tooltip. */
  private val answers = "answers"

  /** The class of the button last pressed in a hint's tooltip. */
  private val chosen = "chosen"

  /** How each tooltip looks, within its own shadow root. */
  private val styles =
    """:host { padding: 3px 8px; border-radius: 6px; background: #0f766e; color: #fff; font: 600 13px/1.4 system-ui, sans-serif; white-space: nowrap; box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3); }
      |""".stripMargin

  /** How a hint's buttons look, the last pressed filled in. */
  private val buttons = s""".$answers { display: flex; gap: 4px; margin-top: 4px; }
       |button { all: initial; padding: 1px 6px; border-radius: 4px; background: rgba(255, 255, 255, 0.18); color: #fff; font: 500 11px/1.4 system-ui, sans-serif; cursor: pointer; }
       |button:hover { background: rgba(255, 255, 255, 0.32); }
       |button.$chosen { background: #fff; color: #0f766e; }
       |button:focus-visible { outline: 2px solid #fff; outline-offset: 1px; }
       |""".stripMargin

  /**
    * Shows the original of each translation pointed at, and the meaning of each
    * hint, until released, and gives `rate` each answer for a hint's word.
    */
  def shown(rate: Rating => Unit): Resource[IO, Tips] =
    Resource.make(IO(Tips(rate)))(tips => IO(tips.stop()))

  private def apply(rate: Rating => Unit): Tips = new Tips(
    Hovering(
      Tooltip(
        document,
        styles,
        interactive = false,
        label = "Original",
      ),
      (hovered, _) =>
        hovered.values.headOption.map(translated => text(translated.original)),
      trusted = true,
    ),
    Hovering(
      Tooltip(
        document,
        styles + buttons,
        label = "Meaning",
      ),
      (hovered, _) => hovered.values.headOption.map(meaning(_, rate)),
      focusable = true,
      trusted = true,
    ),
  )

  private def text(content: String): Node = document.createTextNode(content)

  /**
    * What the tooltip of a hint shows, as `marking` has it: its meaning, or the
    * original of a gloss, and a button for each answer, which gives `rate` the
    * answer for its word.
    */
  private def meaning(marking: Marking, rate: Rating => Unit): Node =
    val content = document.createElement("div")
    content.appendChild(text(marking.tip))
    content.appendChild(row(marking.hint, rate))
    content

  /** A button for each answer to whether the reader knew the word of `hint`. */
  private def row(hint: Hint, rate: Rating => Unit): Element =
    val row = document.createElement("div")
    row.classList.add(answers)
    row.setAttribute("role", "group")
    row.setAttribute(
      "aria-label",
      s"Did you know ${ hint.word }?",
    )
    Answer
      .values
      .foreach(answer =>
        row.appendChild(button(
          row,
          answer,
          () => rate(Rating(hint.word, hint.kind, answer)),
        )),
      )
    row

  /**
    * A button in `row` for `answer`, which gives it with `give`, and shows that
    * it was pressed, the first time the reader presses one in the row, but
    * never for a click that a page script makes.
    */
  private def button
    (
      row: Element,
      answer: Answer,
      give: () => Unit,
    )
    : Element =
    val button = document.createElement("button")
    button.setAttribute("type", "button")
    button.setAttribute("aria-pressed", "false")
    button.textContent = answer.label
    button.addEventListener(
      "click",
      (event: Event) =>
        if event.isTrusted && !answered(row) then
          give()
          press(row, button),
    )
    button

  /** Whether a button in `row` has been pressed. */
  private def answered(row: Element): Boolean =
    row.querySelector("[aria-pressed='true']") != null

  /** Shows that `button` was pressed, of those in `row`. */
  private def press(row: Element, button: Element): Unit = row
    .children
    .foreach(other =>
      other.classList.toggle(chosen, other == button)
      other.setAttribute(
        "aria-pressed",
        (other == button).toString,
      ),
    )
