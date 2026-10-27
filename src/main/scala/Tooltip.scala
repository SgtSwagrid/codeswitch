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
  * in the page's top layer, where nothing in the page can clip or cover it.
  */
object Tooltip:

  /** The class of the tooltip, which `content.css` styles. */
  private[codeswitch] val tooltip = "codeswitch-original"

  /**
    * The space between a word and its tooltip, and kept from the viewport's
    * sides.
    */
  private val gap = 6.0

  /** Shows the original word of each translation pointed at, until released. */
  val shown: Resource[IO, Unit] = Resource
    .make(IO(Tip()))(tip => IO(tip.stop()))
    .void

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

/** The tooltip in the page, holding the original of the translation pointed at. */
private final class Tip:

  private val element = document.createElement("div").asInstanceOf[HTMLElement]
  element.classList.add(Tooltip.tooltip)
  element.setAttribute("popover", "manual")

  /** The translation whose original is shown, if any. */
  private var shown: Option[Element] = None

  /** What to do on each kind of event, anywhere in the page. */
  private val listeners = Map[String, js.Function1[MouseEvent, Unit]](
    "mousemove" -> point,
    "mouseout"  -> (event => if event.relatedTarget == null then hide()),
    "scroll" -> (event => if shown.exists(moved(event.target, _)) then hide()),
  )

  listeners.foreach((kind, listener) =>
    document.addEventListener(kind, listener, true),
  )

  /** Shows the original of the translation pointed at, if any. */
  private def point(event: MouseEvent): Unit =
    Highlighter.translationAt(event.target) match
      case Some(translation) if !shown.contains(translation) =>
        show(
          translation,
          event.clientX,
          event.clientY,
        )
      case Some(_) => ()
      case None    => hide()

  private def show(translation: Element, x: Double, y: Double): Unit =
    element.textContent = Highlighter.tipOf(translation)
    open()
    move(lineAt(translation, x, y))
    shown = Some(translation)

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
