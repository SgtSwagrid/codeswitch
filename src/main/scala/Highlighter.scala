package com.alecdorrington.codeswitch

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import fs2.Stream
import org.scalajs.dom.*
import scala.concurrent.duration.DurationInt
import scala.scalajs.js

/**
  * A word chosen to translate, at `range`, which keeps to the word as the page
  * changes around it, with its `draw` (see [[Drawn]]).
  */
final case class Choice(range: Range, draw: Double, word: Word)

/** That the words of a text have been drawn up to `level` in `pass`. */
private final case class Mark(pass: Int, level: Double)

/**
  * Finds the words of the page to translate, and shows the pieces of their
  * translations, each highlighted, where the language's word order puts it,
  * with the original word shown by [[Tooltip]] on hovering over it. A
  * translation whose draw is above the intensity is hidden, and reads as the
  * original text, until the intensity rises above it.
  */
object Highlighter:

  /** The class of each translation, or piece of one, in the page. */
  private val translated = "codeswitch"

  /** The class of a piece shown away from its word, with a space beside it. */
  private val inserted = "codeswitch-inserted"

  /** The class of the gap left by a word whose translation is elsewhere. */
  private val gap = "codeswitch-gap"

  /**
    * The attribute holding a translation's original word, or the text that a
    * gap was left in place of.
    */
  private val original = "data-codeswitch"

  /**
    * The class added to each part of a translation hidden while the intensity
    * is below its draw, which shows its original text, or nothing if it was
    * inserted.
    */
  private val hidden = "codeswitch-hidden"

  /**
    * The attribute holding what a translation, or a gap, shows while it is not
    * hidden.
    */
  private val translation = "data-codeswitch-translation"

  /** The attribute holding the draw of each part of a translation. */
  private val drawn = "data-codeswitch-draw"

  /** Text that is not prose, or not to be translated. */
  private val skipped = Seq(
    "script",
    "style",
    "noscript",
    "template",
    "textarea",
    "input",
    "select",
    "option",
    "code",
    "pre",
    "kbd",
    "samp",
    "var",
    "svg",
    "math",
    "iframe",
    "[contenteditable]",
    "[translate=no]",
    ".notranslate",
    s".$translated",
    s".$inserted",
    s".$gap",
  ).mkString(", ")

  /** Elements that hold a passage of their own, as a word's context. */
  private val passages = Seq(
    "p",
    "li",
    "dd",
    "dt",
    "td",
    "th",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "blockquote",
    "figcaption",
    "caption",
    "summary",
    "article",
    "section",
    "div",
    "body",
  ).mkString(", ")

  /**
    * The words to translate at `intensity` in the texts beneath `roots` whose
    * words have not been drawn up to it in `pass`, after which they have.
    */
  def choose
    (
      roots: List[Node],
      pass: Int,
      intensity: Double,
    )
    : List[List[Choice]] = roots
    .flatMap(walk(_).filter(readable))
    .distinct
    .map(node => node -> level(node, pass))
    .filter((_, floor) => floor < intensity)
    .map((node, floor) =>
      stamp(node, Mark(pass, intensity))
      choices(node, floor, intensity),
    )
    .filter(_.nonEmpty)

  /** The texts beneath `root`, or `root` itself. */
  private def walk(root: Node): List[Text] =
    val walker =
      document.createTreeWalker(root, NodeFilter.SHOW_TEXT, null, false)
    (Iterator.single(root) ++ Iterator.continually(walker.nextNode()))
      .takeWhile(_ != null)
      .collect { case text: Text => text }
      .toList

  private def readable(text: Text): Boolean = document.body.contains(text) &&
    text.data.trim.nonEmpty && free(text)

  /** Whether `text` is outside all that is not to be translated. */
  private def free(text: Text): Boolean = Option(text.parentNode).exists:
    case parent: Element => parent.closest(skipped) == null
    case _               => false

  /**
    * The words of `node` whose draws are below `intensity`, of those not drawn
    * below `floor` already.
    */
  private def choices
    (
      node: Text,
      floor: Double,
      intensity: Double,
    )
    : List[Choice] = Words
    .choose(
      Words.find(node.data),
      floor,
      intensity,
      () => math.random(),
    )
    .map(drawn =>
      Choice(
        range(
          node,
          drawn.span.start,
          node,
          drawn.span.end,
        ),
        drawn.draw,
        word(node, drawn.span),
      ),
    )

  /**
    * The word at `span` in `node`, with the text around it in its passage, so
    * that a word in a link or in emphasis has the sentence it is part of, as
    * the page had it, without the translations shown in it since.
    */
  private def word(node: Text, span: Span): Word =
    val passage = passageOf(node)
    Word(
      span.in(node.data),
      Words.context(
        untranslated(range(passage, 0, node, span.start)),
        span.in(node.data),
        untranslated(range(
          node,
          span.end,
          passage,
          passage.childNodes.length,
        )),
      ),
    )

  /** The text of `range` as the page had it, without its translations. */
  private def untranslated(range: Range): String =
    val text = range.cloneContents()
    unwrap(text.querySelectorAll)
    text.textContent

  private def passageOf(node: Text): Element = Option(node.parentNode)
    .collect { case parent: Element => parent.closest(passages) }
    .filter(_ != null)
    .getOrElse(document.body)

  /** The range from `start` at `from` up to `end` at `to`. */
  private def range(start: Node, from: Int, end: Node, to: Int): Range =
    val found = document.createRange()
    found.setStart(start, from)
    found.setEnd(end, to)
    found

  /**
    * The intensity up to which the words of `node` have been drawn in `pass`,
    * or `0` if they have not been. Each new choice of language begins a new
    * pass.
    */
  private def level(node: Node, pass: Int): Double = markOf(node)
    .toOption
    .filter(_.pass == pass)
    .fold(0.0)(_.level)

  private def markOf(node: Node): js.UndefOr[Mark] = node
    .asInstanceOf[js.Dynamic]
    .codeswitch
    .asInstanceOf[js.UndefOr[Mark]]

  private def stamp(node: Node, mark: js.UndefOr[Mark]): Unit =
    node.asInstanceOf[js.Dynamic].codeswitch = mark.asInstanceOf[js.Any]

  /**
    * Shows each of `translations` around the word that it translates, among
    * `choices`, unless the word has changed since it was chosen, or the
    * translation cannot all be shown, hidden unless its draw is below
    * `intensity`. Each text split off another keeps its mark.
    */
  def show
    (
      choices: List[Choice],
      translations: List[Translation],
      intensity: Double,
    )
    : Unit = choices
    .zip(translations)
    .foreach((choice, translation) => place(choice, translation, intensity))

  /**
    * Shows `translation` around the word of `choice`, laid out by [[Placement]]
    * in the text of its passage as it is now, each part holding the draw that
    * decides whether it is shown at `intensity` (see [[drawAcross]]).
    */
  private def place
    (
      choice: Choice,
      translation: Translation,
      intensity: Double,
    )
    : Unit = for
    node <- wordOf(choice)
    passage = Passage(walk(passageOf(node)).filter(free))
    within <- passage.spanOf(node, 0, node.length)
    word   <- passage.spanOf(
      node,
      choice.range.startOffset,
      choice.range.endOffset,
    )
    layout <- Placement.lay(passage.text, within, word, translation)
    edits  <- edits(passage, layout)
    draw = drawAcross(
      passage,
      layout,
      choice.draw,
      passageOf(node),
    )
  do
    edits.foreach((range, element) =>
      element.setAttribute(drawn, draw.toString)
      put(range, element)
      if draw >= intensity then toggle(element),
    )

  /**
    * The draw that decides whether a translation laid out as `layout` in
    * `passage`, beneath `root`, is shown: the `draw` of its word, or that of
    * any translation between the word and the space beside it that it takes
    * away, if greater. The space was chosen with that translation shown, so the
    * two are shown or hidden together, lest they leave no space between words.
    */
  private def drawAcross
    (
      passage: Passage,
      layout: Layout,
      draw: Double,
      root: Element,
    )
    : Double = layout
    .space
    .map(space =>
      if space.start < layout.home.start then (space, layout.home)
      else (layout.home, space),
    )
    .flatMap((first, last) =>
      (passage.range(first), passage.range(last)).mapN((first, last) =>
        range(
          first.endContainer,
          first.endOffset,
          last.startContainer,
          last.startOffset,
        ),
      ),
    )
    .fold(draw)(between =>
      root
        .querySelectorAll(s"[$drawn]")
        .filter(within(between, _))
        .map(drawOf)
        .foldLeft(draw)(_ max _),
    )

  /**
    * Whether `node` is at least partly within `range`, which the DOM's
    * `Range.intersectsNode` tells, as scala-js-dom does not declare it.
    */
  private def within(range: Range, node: Node): Boolean = range
    .asInstanceOf[js.Dynamic]
    .intersectsNode(node)
    .asInstanceOf[Boolean]

  /** The text that the word of `choice` is in, unless the word has changed. */
  private def wordOf(choice: Choice): Option[Text] = Option(
    choice.range.startContainer,
  ).collect {
      case node: Text
        if node == choice.range.endContainer && document.body.contains(node) =>
        node
    }
    .filter(_ => choice.range.toString == choice.word.text)

  /**
    * Where each part of `layout` goes in `passage`, all found before any is put
    * there, with what is shown there. They are in order from the end of the
    * passage back, a replacement before an insertion at the same place, so that
    * putting each in place leaves the places of the rest as they were.
    */
  private def edits
    (passage: Passage, layout: Layout)
    : Option[List[(Range, Element)]] =
    val word = layout.home.in(passage.text)
    ((layout.home, passage.range(layout.home), home(word, layout.shown)) ::
      layout
        .space
        .toList
        .map(space =>
          (space, passage.range(space), gapFor(space.in(passage.text))),
        ) ++
      layout
        .inserted
        .map(insert =>
          (
            Span(insert.at, insert.at),
            passage.point(insert.at, insert.after),
            piece(word, insert),
          ),
        ))
      .reverse
      .sortBy((span, _, _) => (-span.start, span.length == 0))
      .traverse((_, range, element) => range.map(_ -> element))

  /** What is shown in place of `word`: `shown`, or a gap if nothing is. */
  private def home(word: String, shown: String): Element =
    if shown.isEmpty then gapFor(word) else highlight(word, shown)

  /** `insert`, a piece of the translation of `word`, with a space beside it. */
  private def piece(word: String, insert: Insert): Element =
    val element = marked(inserted)
    val parts   = List[Node](
      document.createTextNode(" "),
      highlight(word, insert.piece),
    )
    (if insert.after then parts else parts.reverse).foreach(element.appendChild)
    element

  private def highlight(word: String, text: String): Element =
    showing(marked(translated, word), text)

  /** The gap left in place of `text`. */
  private def gapFor(text: String): Element = showing(marked(gap, text), "")

  /** A span of the class `kind`, holding the original `text`, if any. */
  private def marked(kind: String, text: String = ""): Element =
    val element = document.createElement("span")
    element.classList.add(kind)
    if text.nonEmpty then element.setAttribute(original, text)
    element

  /** `element`, showing `text`, and holding it to show while not hidden. */
  private def showing(element: Element, text: String): Element =
    element.setAttribute(translation, text)
    element.textContent = text
    element

  /**
    * Puts `element` in place of the text in `range`, splitting it from the rest
    * of its text, which keeps the text's mark.
    */
  private def put(range: Range, element: Element): Unit =
    val node = range.startContainer
    range.deleteContents()
    range.insertNode(element)
    Option(element.nextSibling).foreach(stamp(_, markOf(node)))

  /** The translation that `target` is within, if it is within one shown. */
  def translationAt(target: EventTarget): Option[Element] = Option(target)
    .collect { case element: Element =>
      element.closest(s".$translated:not(.$hidden)")
    }
    .filter(_ != null)

  /** The original word of `translation`. */
  def originalOf(translation: Element): String =
    translation.getAttribute(original)

  /**
    * Shows each translation whose draw is below `intensity`, and hides the
    * rest, so that each reads as its original text.
    */
  def reveal(intensity: Double): Unit = document
    .querySelectorAll(s"[$drawn]")
    .filter(element =>
      (drawOf(element) >= intensity) != element.classList.contains(hidden),
    )
    .foreach(toggle)

  /**
    * The draw that `element` holds, or `0` if it holds none, read by the page's
    * own `parseFloat`, which keeps Scala's parser out of the pages.
    */
  private def drawOf(element: Element): Double =
    val draw = js.Dynamic.global.parseFloat(element.getAttribute(drawn))
    Option(draw.asInstanceOf[Double]).filterNot(_.isNaN).getOrElse(0.0)

  /**
    * Hides `element`, a part of a translation, if it is shown, or else shows
    * it, putting in it the text that it holds for that.
    */
  private def toggle(element: Element): Unit =
    val hide = !element.classList.contains(hidden)
    element.classList.toggle(hidden, hide)
    if element.classList.contains(inserted) then stow(element, hide)
    else
      element.textContent =
        element.getAttribute(if hide then original else translation)

  /**
    * Takes the parts of `piece`, shown away from its word, out of the page if
    * `hide`, keeping them on it, or else puts them back, so that nothing that
    * reads the page finds a hidden piece's text.
    */
  private def stow(piece: Element, hide: Boolean): Unit =
    val kept = piece.asInstanceOf[js.Dynamic]
    if hide then
      val parts = document.createDocumentFragment()
      piece.childNodes.toList.foreach(parts.appendChild)
      kept.codeswitchParts = parts
    else
      kept
        .codeswitchParts
        .asInstanceOf[js.UndefOr[DocumentFragment]]
        .foreach(piece.appendChild)

  /**
    * Puts the original words back in place of their translations, and takes
    * away the pieces shown elsewhere.
    */
  def restore(): Unit = unwrap(document.querySelectorAll)

  /**
    * Puts the original words back in place of their translations among the
    * elements that `select` finds, and takes away the pieces shown elsewhere.
    */
  private def unwrap(select: String => NodeList[Element]): Unit =
    select(s".$inserted").foreach(element =>
      element.parentNode.removeChild(element),
    )
    select(s".$translated, .$gap").foreach(element =>
      element
        .parentNode
        .replaceChild(
          document.createTextNode(element.getAttribute(original)),
          element,
        ),
    )

  /**
    * At once, then each second, the page's body if `whole` says so, or else the
    * nodes added to it since the last time, if any.
    */
  def nodes(whole: IO[Boolean]): Stream[IO, List[Node]] = Stream
    .resource(
      Resource.make(IO(Additions(document.body)))(added => IO(added.stop())),
    )
    .flatMap(added =>
      (Stream.unit ++ Stream.awakeEvery[IO](1.second).void)
        .evalMap(_ =>
          whole.flatMap(body =>
            IO(added.drain()).map(nodes =>
              if body then List[Node](document.body) else nodes,
            ),
          ),
        )
        .filter(_.nonEmpty),
    )

  /** Whether the page is out of sight, as in a tab in the background. */
  def outOfSight: Boolean = document.visibilityState == VisibilityState.hidden

  /**
    * The texts of a passage, in order, among which the pieces of a translation
    * are shown, as one `text`.
    */
  private final class Passage(texts: List[Text]):

    private val nodes = texts.toVector

    /** Where each of the texts begins in `text`, then where the last ends. */
    private val starts = nodes.scanLeft(0)(_ + _.length)

    val text: String = nodes.map(_.data).mkString

    /** The span of `node` in `text`, from `from` up to `to` within it. */
    def spanOf(node: Text, from: Int, to: Int): Option[Span] =
      Some(nodes.indexOf(node))
        .filter(_ >= 0)
        .map(index => Span(starts(index) + from, starts(index) + to))

    /** The range of `span` of `text`. */
    def range(span: Span): Option[Range] =
      for
        (start, from) <- position(span.start, false)
        (end, to)     <- position(span.end, true)
      yield Highlighter.range(start, from, end, to)

    /**
      * The empty range `at` a place in `text`, at the end of the text before it
      * if `after`, or else at the start of the text after it.
      */
    def point(at: Int, after: Boolean): Option[Range] = position(at, after).map(
      (node, offset) => Highlighter.range(node, offset, node, offset),
    )

    private def position(at: Int, after: Boolean): Option[(Text, Int)] = nodes
      .indices
      .find(index =>
        if after then starts(index) < at && at <= starts(index + 1)
        else starts(index) <= at && at < starts(index + 1),
      )
      .map(index => nodes(index) -> (at - starts(index)))

/** The nodes added beneath `root` since they were last drained. */
private final class Additions(root: Node):

  private val added = js.Array[Node]()

  private val observer = MutationObserver((records, _) =>
    records.foreach(record =>
      (0 until record.addedNodes.length).foreach(index =>
        added.push(record.addedNodes(index)),
      ),
    ),
  )

  observer.observe(
    root,
    new MutationObserverInit:
      childList = true
      subtree = true,
  )

  /** The nodes added since this was last called. */
  def drain(): List[Node] =
    val nodes = added.toList
    added.length = 0
    nodes

  /** Stops gathering nodes. */
  def stop(): Unit = observer.disconnect()
