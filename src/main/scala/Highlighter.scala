package com.alecdorrington.codeswitch

import cats.effect.{IO, Resource}
import fs2.Stream
import org.scalajs.dom.*
import scala.concurrent.duration.DurationInt
import scala.scalajs.js

/**
  * A word chosen to translate at `span` in the text `node`, whose text was
  * `data` when it was chosen.
  */
final case class Choice
  (
    node: Text,
    data: String,
    span: Span,
    word: Word,
  )

/**
  * Finds the words of the page to translate, and shows their translations, each
  * highlighted, with the original word shown above it on hovering over it.
  */
object Highlighter:

  /** The class of each translation shown in the page. */
  private val translated = "codeswitch"

  /** The attribute holding a translation's original word. */
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
    .flatMap(texts)
    .distinct
    .filterNot(seen(_, pass))
    .map(node =>
      see(node, pass)
      choices(node),
    )
    .filter(_.nonEmpty)

  /** The texts beneath `root`, or `root` itself, which may be translated. */
  private def texts(root: Node): List[Text] =
    val walker =
      document.createTreeWalker(root, NodeFilter.SHOW_TEXT, null, false)
    (Iterator.single(root) ++ Iterator.continually(walker.nextNode()))
      .takeWhile(_ != null)
      .collect { case text: Text if readable(text) => text }
      .toList

  private def readable(text: Text): Boolean = document.body.contains(text) &&
    text.data.trim.nonEmpty && Option(text.parentNode).exists:
      case parent: Element => parent.closest(skipped) == null
      case _               => false

  private def choices(node: Text): List[Choice] = Words
    .choose(
      Words.find(node.data),
      () => math.random(),
    )
    .map(span => Choice(node, node.data, span, word(node, span)))

  /**
    * The word at `span` in `node`, with the text around it in its passage, so
    * that a word in a link or in emphasis has the sentence it is part of.
    */
  private def word(node: Text, span: Span): Word =
    val passage = passageOf(node)
    Word(
      span.in(node.data),
      Words.context(
        between(passage, 0, node, span.start),
        span.in(node.data),
        between(
          node,
          span.end,
          passage,
          passage.childNodes.length,
        ),
      ),
    )

  private def passageOf(node: Text): Node = Option(node.parentNode)
    .collect { case parent: Element => parent.closest(passages) }
    .filter(_ != null)
    .getOrElse(document.body)

  /** The text from `start` at `from` up to `end` at `to`. */
  private def between(start: Node, from: Int, end: Node, to: Int): String =
    val range = document.createRange()
    range.setStart(start, from)
    range.setEnd(end, to)
    range.toString

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
    * Shows each of `translations` in place of the word that it translates,
    * among `choices`, of one text, unless the text has changed since. Each text
    * that it is split into has been looked at in `pass`.
    */
  def show
    (
      choices: List[Choice],
      translations: List[Option[String]],
      pass: Int,
    )
    : Unit = choices
    .zip(translations)
    .collect { case (choice, Some(translation)) => choice -> translation }
    .filter((choice, _) => unchanged(choice))
    .sortBy((choice, _) => -choice.span.start)
    .foreach((choice, translation) => replace(choice, translation, pass))

  private def unchanged(choice: Choice): Boolean = document
    .body
    .contains(choice.node) && choice.node.data == choice.data

  /** Splits the word from its text, then replaces it with its translation. */
  private def replace
    (
      choice: Choice,
      translation: String,
      pass: Int,
    )
    : Unit =
    see(
      choice.node.splitText(choice.span.end),
      pass,
    )
    val word = choice.node.splitText(choice.span.start)
    word.parentNode.replaceChild(highlight(word.data, translation), word)

  private def highlight(word: String, translation: String): Element =
    val element = document.createElement("span")
    element.classList.add(translated)
    element.setAttribute(original, word)
    element.textContent = Words.matchCase(word, translation)
    element

  /** Puts the original words back in place of their translations. */
  def restore(): Unit = document
    .querySelectorAll(s".$translated")
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
