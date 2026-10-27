package com.alecdorrington.codeswitch

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import org.scalajs.dom.*
import scala.scalajs.js

/** A box in the viewport, of `width` and `height`, in pixels. */
final case class Box
  (
    left: Double,
    top: Double,
    width: Double,
    height: Double,
  ):

  def right: Double = left + width

  def bottom: Double = top + height

  /** Whether the point at `x` and `y` is within this box. */
  def contains(x: Double, y: Double): Boolean = left <= x && x <= right &&
    top <= y && y <= bottom

  /** This box, moved to `left` and `top`. */
  def at(left: Double, top: Double): Box = copy(left = left, top = top)

  /** The least box that holds both this box and `other`. */
  def union(other: Box): Box =
    val (left, top) = (this.left.min(other.left), this.top.min(other.top))
    Box(
      left,
      top,
      right.max(other.right) - left,
      bottom.max(other.bottom) - top,
    )

object Box:

  /** The box of `rect`. */
  def apply(rect: DOMRect): Box = Box(
    rect.left,
    rect.top,
    rect.width,
    rect.height,
  )

/**
  * Shows the original word of the translation under the pointer, or the meaning
  * of the hint, above it, or below it where there is no room above. It is shown
  * in the page's top layer, where nothing in the page can clip or cover it. A
  * hint's tooltip has a button for each [[Answer]] to whether the reader knew
  * its word, and stays open while the pointer goes from the word to it.
  */
object Tooltip:

  /** The class of the tooltip, which `content.css` styles. */
  private[codeswitch] val tooltip = "codeswitch-original"

  /**
    * The space between a word and its tooltip, and kept from the viewport's
    * sides.
    */
  private val gap = 6.0

  /** The class of the tooltip of a hint, which may be pointed at and clicked. */
  private[codeswitch] val rating = "codeswitch-rating"

  /** The class of the row of buttons in a hint's tooltip. */
  private[codeswitch] val answers = "codeswitch-answers"

  /** The class of the button last pressed in a hint's tooltip. */
  private[codeswitch] val chosen = "codeswitch-chosen"

  /**
    * Shows the original word of each translation pointed at, or the meaning of
    * each hint, until released, and gives `rate` each answer for a hint's word.
    */
  def shown(rate: Rating => Unit): Resource[IO, Unit] = Resource
    .make(IO(Tip(rate)))(tip => IO(tip.stop()))
    .void

  /**
    * Whether the pointer at `x` and `y` is on its way between `word` and its
    * `tip`, or on either, so that the tip stays open for it to reach.
    */
  def bridges(word: Box, tip: Box, x: Double, y: Double): Boolean = word
    .union(tip)
    .contains(x, y)

  /**
    * Where to put `tip`: centred above `word`, or below it where there is no
    * room above, and within the sides of `viewport`.
    */
  def place(word: Box, tip: Box, viewport: Box): Box = tip.at(
    (word.left + (word.width - tip.width) / 2)
      .min(viewport.right - gap - tip.width)
      .max(viewport.left + gap),
    if word.top - gap - tip.height >= viewport.top then
      word.top - gap - tip.height
    else word.bottom + gap,
  )

/**
  * The tooltip in the page, holding the original of the translation pointed at,
  * or the meaning of the hint, which gives `rate` each answer for a hint.
  */
private final class Tip(rate: Rating => Unit):

  private val element = document.createElement("div").asInstanceOf[HTMLElement]
  element.classList.add(Tooltip.tooltip)
  element.setAttribute("popover", "manual")

  /** The translation whose original is shown, if any. */
  private var shown: Option[Element] = None

  /** What to do on each kind of event, anywhere in the page. */
  private val listeners = Map[String, js.Function1[MouseEvent, Unit]](
    "mousemove" -> (event => if event.isTrusted then point(event)),
    "mouseout"  -> (event => if event.relatedTarget == null then hide()),
    "scroll" -> (event => if shown.exists(moved(event.target, _)) then hide()),
  )

  listeners.foreach((kind, listener) =>
    document.addEventListener(kind, listener, true),
  )

  /**
    * The pointer's and the mouse's events on the tooltip, which stop there, so
    * that the page's own handlers neither hear them nor lose focus to them.
    */
  private val kept = Seq(
    "pointerdown",
    "pointerup",
    "mousedown",
    "mouseup",
    "click",
    "dblclick",
    "contextmenu",
  )

  kept.foreach(kind =>
    element.addEventListener(
      kind,
      (event: Event) =>
        event.stopPropagation()
        if kind == "mousedown" then event.preventDefault(),
    ),
  )

  /**
    * Whether the reader has answered for the word of the tooltip shown, which
    * counts only once each time it is shown.
    */
  private var answered = false

  /** The box of the line of the word whose tooltip is shown, if any. */
  private var word: Option[Box] = None

  /**
    * Shows the tooltip of the translation or hint pointed at, if any, or keeps
    * a hint's open while the pointer is on its way to it, or on it.
    */
  private def point(event: MouseEvent): Unit =
    Highlighter.translationAt(event.target) match
      case Some(translation) if !shown.contains(translation) =>
        show(
          translation,
          event.clientX,
          event.clientY,
        )
      case Some(_)                                        => ()
      case None if reaching(event.clientX, event.clientY) => ()
      case None                                           => hide()

  /** Whether the pointer at `x` and `y` is on its way to a hint's tooltip. */
  private def reaching(x: Double, y: Double): Boolean = element
    .classList
    .contains(Tooltip.rating) && word.exists(Tooltip.bridges(
      _,
      Box(element.getBoundingClientRect()),
      x,
      y,
    ))

  private def show(translation: Element, x: Double, y: Double): Unit =
    element.replaceChildren(
      document.createTextNode(Highlighter.tipOf(translation)),
    )
    val rated = Highlighter.ratingOf(translation)
    element.classList.toggle(Tooltip.rating, rated.nonEmpty)
    rated.foreach((word, kind) => element.appendChild(buttons(word, kind)))
    open()
    word = Some(lineAt(translation, x, y))
    word.foreach(move)
    shown = Some(translation)
    answered = false

  /**
    * A button for each answer to whether the reader knew `word`, shown as
    * `kind`.
    */
  private def buttons(word: String, kind: Hint.Kind): Element =
    val row = document.createElement("div")
    row.classList.add(Tooltip.answers)
    row.setAttribute("role", "group")
    row.setAttribute("aria-label", s"Did you know $word?")
    Answer.values.foreach(answer => row.appendChild(button(word, kind, answer)))
    row

  /**
    * A button for `answer`, which gives `rate` the answer for `word`, and shows
    * that it was pressed, the first time the reader presses one since the
    * tooltip was shown, but never for a click that a page script makes.
    */
  private def button
    (
      word: String,
      kind: Hint.Kind,
      answer: Answer,
    )
    : Element =
    val button = document.createElement("button")
    button.setAttribute("type", "button")
    button.setAttribute("aria-pressed", "false")
    button.textContent = answer.label
    button.addEventListener(
      "click",
      (event: Event) =>
        if event.isTrusted && !answered then
          answered = true
          rate(Rating(word, kind, answer))
          button
            .parentNode
            .childNodes
            .foreach {
              case other: Element =>
                other.classList.toggle(Tooltip.chosen, other == button)
                other.setAttribute(
                  "aria-pressed",
                  (other == button).toString,
                )
              case _ => ()
            },
    )
    button

  /** Puts the tooltip in the top layer, in case it is not there. */
  private def open(): Unit =
    if !element.isConnected then document.documentElement.appendChild(element)
    if !isOpen then element.asInstanceOf[Popover].showPopover()

  /** Moves the tooltip to `word`, once it is open, so that its size is known. */
  private def move(word: Box): Unit =
    val placed = Tooltip.place(
      word,
      Box(element.getBoundingClientRect()),
      viewport,
    )
    Seq("left" -> placed.left, "top" -> placed.top).foreach((side, at) =>
      element.style.setProperty(side, s"${ at }px", "important"),
    )

  /** The viewport, without its scroll bars. */
  private def viewport: Box = Box(
    0,
    0,
    document.documentElement.clientWidth,
    document.documentElement.clientHeight,
  )

  /**
    * The box of the line of `translation` at `x` and `y`, as a translation may
    * wrap onto the next line.
    */
  private def lineAt(translation: Element, x: Double, y: Double): Box =
    translation
      .getClientRects()
      .map(Box(_))
      .find(_.contains(x, y))
      .getOrElse(Box(translation.getBoundingClientRect()))

  /**
    * Whether scrolling `target`, the page or an element in it, moves
    * `translation`.
    */
  private def moved
    (
      target: EventTarget,
      translation: Element,
    )
    : Boolean = target match
    case node: Node => node.contains(translation)
    case _          => false

  private def hide(): Unit =
    if shown.nonEmpty && isOpen then element.asInstanceOf[Popover].hidePopover()
    shown = None
    word = None

  private def isOpen: Boolean = element.matches(":popover-open")

  /** Removes the tooltip, and stops following the pointer. */
  def stop(): Unit =
    listeners.foreach((kind, listener) =>
      document.removeEventListener(kind, listener, true),
    )
    element.remove()

/** An element that may be shown in the page's top layer, above all else. */
@js.native
private trait Popover extends js.Object:

  def showPopover(): Unit = js.native

  def hidePopover(): Unit = js.native
