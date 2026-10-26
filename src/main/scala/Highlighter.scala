package com.alecdorrington.codeswitch

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import fs2.Stream
import org.scalajs.dom.*
import scala.concurrent.duration.DurationInt
import scala.scalajs.js

/**
  * A word chosen to translate, at `range`, which keeps to the word as the page
  * changes around it.
  */
final case class Choice(range: Range, word: Word)

/**
  * Finds the words of the page to translate, and shows the pieces of their
  * translations, each highlighted, where the language's word order puts it,
  * with the original word shown by [[Tooltip]] on hovering over it.
  */
object Highlighter:

  /** The class of each translation, or piece of one, shown in the page. */
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
    * The words to translate in the texts beneath `roots`, which have not been
    * looked at in `pass`, after which they have.
    */
  def choose(roots: List[Node], pass: Int): List[List[Choice]] = roots
    .flatMap(walk(_).filter(readable))
    .distinct
    .filterNot(seen(_, pass))
    .map(node =>
      see(node, pass)
      choices(node),
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

  private def choices(node: Text): List[Choice] = Words
    .choose(
      Words.find(node.data),
      () => math.random(),
    )
    .map(span =>
      Choice(
        range(node, span.start, node, span.end),
        word(node, span),
      ),
    )

  /**
    * The word at `span` in `node`, with the text around it in its passage, so
    * that a word in a link or in emphasis has the sentence it is part of.
    */
  private def word(node: Text, span: Span): Word =
    val passage = passageOf(node)
    Word(
      span.in(node.data),
      Words.context(
        range(passage, 0, node, span.start).toString,
        span.in(node.data),
        range(
          node,
          span.end,
          passage,
          passage.childNodes.length,
        ).toString,
      ),
    )

  private def passageOf(node: Text): Node = Option(node.parentNode)
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
    * Whether `node` has been looked at in `pass`, or was split off one that has
    * been. Each new choice of language begins a new pass.
    */
  private def seen(node: Node, pass: Int): Boolean = node
    .asInstanceOf[js.Dynamic]
    .codeswitch
    .asInstanceOf[js.UndefOr[Int]] == js.defined(pass)

  private def see(node: Node, pass: Int): Unit =
    node.asInstanceOf[js.Dynamic].codeswitch = pass

  /**
    * Shows each of `translations` around the word that it translates, among
    * `choices`, unless the word has changed since it was chosen, or the
    * translation cannot all be shown. Each text split off one that has been
    * looked at in `pass` has been too.
    */
  def show
    (
      choices: List[Choice],
      translations: List[Translation],
      pass: Int,
    )
    : Unit = choices
    .zip(translations)
    .foreach((choice, translation) => place(choice, translation, pass))

  /**
    * Shows `translation` around the word of `choice`, laid out by [[Placement]]
    * in the text of its passage as it is now.
    */
  private def place
    (
      choice: Choice,
      translation: Translation,
      pass: Int,
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
  do edits.foreach((range, element) => put(range, element, pass))

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
          (space, passage.range(space), marked(gap, space.in(passage.text))),
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
    if shown.isEmpty then marked(gap, word) else highlight(word, shown)

  /** `insert`, a piece of the translation of `word`, with a space beside it. */
  private def piece(word: String, insert: Insert): Element =
    val element = marked(inserted)
    val parts   = List[Node](
      document.createTextNode(" "),
      highlight(word, insert.piece),
    )
    (if insert.after then parts else parts.reverse).foreach(element.appendChild)
    element

  private def highlight(word: String, translation: String): Element =
    val element = marked(translated, word)
    element.textContent = translation
    element

  /** A span of the class `kind`, holding the original `text`, if any. */
  private def marked(kind: String, text: String = ""): Element =
    val element = document.createElement("span")
    element.classList.add(kind)
    if text.nonEmpty then element.setAttribute(original, text)
    element

  /**
    * Puts `element` in place of the text in `range`, splitting it from the rest
    * of its text, which has been looked at in `pass` if the text had been.
    */
  private def put(range: Range, element: Element, pass: Int): Unit =
    val node = range.startContainer
    range.deleteContents()
    range.insertNode(element)
    Option(element.nextSibling)
      .filter(_ => seen(node, pass))
      .foreach(see(_, pass))

  /** The translation that `target` is within, if it is within one. */
  def translationAt(target: EventTarget): Option[Element] = Option(target)
    .collect { case element: Element => element.closest(s".$translated") }
    .filter(_ != null)

  /** The original word of `translation`. */
  def originalOf(translation: Element): String =
    translation.getAttribute(original)

  /**
    * Puts the original words back in place of their translations, and takes
    * away the pieces shown elsewhere.
    */
  def restore(): Unit =
    document
      .querySelectorAll(s".$inserted")
      .foreach(element => element.parentNode.removeChild(element))
    document
      .querySelectorAll(s".$translated, .$gap")
      .foreach(element =>
        element
          .parentNode
          .replaceChild(
            document.createTextNode(element.getAttribute(original)),
            element,
          ),
      )

  /**
    * The page's body, then the nodes added to it, gathered each second, from
    * the moment that the body is emitted.
    */
  def nodes: Stream[IO, List[Node]] = Stream
    .resource(
      Resource.make(IO(Additions(document.body)))(added => IO(added.stop())),
    )
    .flatMap(added =>
      Stream.emit(List(document.body)) ++
        Stream
          .awakeEvery[IO](1.second)
          .evalMap(_ => IO(added.drain()))
          .filter(_.nonEmpty),
    )

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
