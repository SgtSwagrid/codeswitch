package com.alecdorrington.codeswitch

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import fs2.Stream
import org.scalajs.dom.*
import scala.concurrent.duration.DurationInt
import scala.scalajs.js
import scala.util.Try

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

  /** Elements whose text is what is clicked or read, so must not be emptied. */
  private val labels = Seq(
    "a",
    "button",
    "label",
    "summary",
    "[role=button]",
    "[role=link]",
    "[role=tab]",
    "[role=menuitem]",
  ).mkString(", ")

  /** What stands for an element in a passage's text, of which none is prose. */
  private val placeholder = '￼'

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
    : List[Choice] =
    lazy val passage = Passage(passageOf(node))
    Words
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
          word(passage, node, drawn.span),
        ),
      )

  /**
    * The word at `span` in `node`, with the text around it in its `passage`, so
    * that a word in a link or in emphasis has the sentence it is part of. The
    * text is the page's own, as it was before any of it was translated, and
    * without what is not to be translated, as code, scripts and text fields.
    */
  private def word(passage: Passage, node: Text, span: Span): Word =
    val (before, after) = passage.around(node, span)
    Word(
      span.in(node.data),
      Words.context(before, span.in(node.data), after),
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
    * translation cannot all be shown. They are shown from the last back, so
    * that each is shown before those before it change the text around it, and
    * each apart from the rest, so that one that fails leaves them be. Each is
    * hidden unless its draw is below `intensity`. Each text split off another
    * keeps its mark.
    */
  def show
    (
      choices: List[Choice],
      translations: List[Translation],
      intensity: Double,
    )
    : Unit = choices
    .zip(translations)
    .reverse
    .foreach((choice, translation) =>
      Try(place(choice, translation, intensity))
        .failed
        .foreach(error => console.warn(s"Codeswitch: ${ error.getMessage }")),
    )

  /**
    * Shows `translation` around the word of `choice`, laid out by [[Placement]]
    * in the text of its passage as it is now, unless that would empty a link,
    * each part holding the word's draw, and hidden unless that is below
    * `intensity`.
    */
  private def place
    (
      choice: Choice,
      translation: Translation,
      intensity: Double,
    )
    : Unit = for
    node <- wordOf(choice)
    passage = Passage(passageOf(node))
    within <- passage.spanOf(node, 0, node.length)
    word   <- passage.spanOf(
      node,
      choice.range.startOffset,
      choice.range.endOffset,
    )
    layout <- Placement.lay(passage.text, within, word, translation)
    if !empties(node, passage, layout)
    edits <- edits(passage, layout)
  do
    edits.foreach(edit =>
      val parts = elementsOf(edit.element)
      parts.foreach(_.setAttribute(drawn, choice.draw.toString))
      make(edit)
      if choice.draw >= intensity then parts.foreach(toggle),
    )

  /** The elements that `node` puts in the page: itself, or a fragment's. */
  private def elementsOf(node: Node): List[Element] = node match
    case element: Element => List(element)
    case _                =>
      node.childNodes.toList.collect { case element: Element => element }

  /**
    * The text that the word of `choice` is in, unless the word has changed. An
    * element put where the range begins is put within it, as the range begins
    * at the end of the text before it, so its start is first moved past it.
    */
  private def wordOf(choice: Choice): Option[Text] =
    settle(choice.range)
    Option(choice.range.startContainer)
      .collect {
        case node: Text
          if node == choice.range.endContainer &&
          document.body.contains(node) => node
      }
      .filter(_ => choice.range.toString == choice.word.text)

  /** Moves `range` from the end of the text it begins in to the next. */
  private def settle(range: Range): Unit = range.startContainer match
    case node: Text
      if range.startOffset == node.length && node != range.endContainer =>
      following(node).foreach(range.setStart(_, 0))
    case _ => ()

  /** The next text after `node` with prose in it. */
  private def following(node: Text): Option[Text] =
    val walker = document.createTreeWalker(
      document.body,
      NodeFilter.SHOW_TEXT,
      null,
      false,
    )
    walker.currentNode = node
    Iterator
      .continually(walker.nextNode())
      .takeWhile(_ != null)
      .collectFirst { case text: Text if text.length > 0 && free(text) => text }

  /**
    * Whether `layout` would take away all the text of a link, button or label
    * that `node` is in, leaving nothing in it to click or read.
    */
  private def empties
    (
      node: Text,
      passage: Passage,
      layout: Layout,
    )
    : Boolean = layout.shown.isEmpty && Option(node.parentNode)
    .collect { case parent: Element => parent.closest(labels) }
    .filter(_ != null)
    .exists(_.textContent.trim == layout.home.in(passage.text).trim)

  /**
    * `element`, in place of the text of `node` from `from` up to `to`, or put
    * at `from` if that is `to`.
    */
  private final case class Edit
    (
      node: Text,
      from: Int,
      to: Int,
      element: Node,
    )

  /**
    * Where each part of `layout` goes in `passage`, all found before any is put
    * there, with what is shown there. They are in order from the end of the
    * passage back, a replacement before an insertion at the same place, so that
    * putting each in place leaves the places of the rest as they were. The
    * pieces put in one place go in together, in their own order.
    */
  private def edits(passage: Passage, layout: Layout): Option[List[Edit]] =
    val word         = layout.home.in(passage.text)
    val replacements = (layout.home -> home(word, layout.shown)) ::
      layout.space.toList.map(space => space -> gapFor(space.in(passage.text)))
    val insertions = layout
      .inserted
      .groupBy(insert => (insert.at, insert.after))
      .toList
      .map:
        case ((at, after), inserts) => (
            at,
            true,
            passage
              .at(at, after)
              .map((node, offset) =>
                Edit(
                  node,
                  offset,
                  offset,
                  together(inserts.map(piece(word, _))),
                ),
              ),
          )
    (replacements.map((span, element) =>
      (
        span.start,
        false,
        passage
          .within(span)
          .map((node, from, to) => Edit(node, from, to, element)),
      ),
    ) ++ insertions)
      .sortBy((at, insertion, _) => (-at, insertion))
      .traverse(_._3)

  /** `elements`, in order, to be put in one place at once. */
  private def together(elements: List[Element]): Node =
    val fragment = document.createDocumentFragment()
    elements.foreach(fragment.appendChild)
    fragment

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

  /** Puts the element of `edit` in place of its text, or at its place. */
  private def make(edit: Edit): Unit =
    if edit.from == edit.to then insert(edit.node, edit.from, edit.element)
    else
      replace(
        edit.node,
        edit.from,
        edit.to,
        edit.element,
      )

  /** Puts `element` at `at` in `node`, or beside it, if `at` is at an end. */
  private def insert(node: Text, at: Int, element: Node): Unit = node
    .parentNode
    .insertBefore(
      element,
      if at == 0 then node else split(node, at).getOrElse(node.nextSibling),
    )

  /**
    * Puts `element` in place of the text from `from` up to `to` in `node`. If
    * that is where `node` begins, `node` itself stays after it, empty, as the
    * page may hold on to it; [[restore]] puts the text back into it.
    */
  private def replace
    (
      node: Text,
      from: Int,
      to: Int,
      element: Node,
    )
    : Unit =
    split(node, to)
    derive(element, node)
    split(node, from) match
      case Some(word) => word.parentNode.replaceChild(element, word)
      case None       =>
        node.parentNode.insertBefore(element, node)
        node.data = ""

  /**
    * The text after `at` in `node`, split off it, unless `at` is at an end. It
    * is from the same text of the page as `node`, and keeps its mark, so that
    * its words are not drawn again.
    */
  private def split(node: Text, at: Int): Option[Text] =
    Option.when(0 < at && at < node.length):
      val rest = node.splitText(at)
      derive(rest, node)
      stamp(rest, markOf(node))
      rest

  /** The text of the page that `node` was split from, or put in place of. */
  private def originOf(node: Node): Option[Text] = node
    .asInstanceOf[js.Dynamic]
    .codeswitchOrigin
    .asInstanceOf[js.UndefOr[Text]]
    .toOption

  /** Marks `node` as from the same text of the page as `text`, and `text` too. */
  private def derive(node: Node, text: Text): Unit =
    val origin = originOf(text).getOrElse(text)
    Seq[Node](text, node).foreach(_.asInstanceOf[js.Dynamic].codeswitchOrigin =
      origin,
    )

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
    * Puts the original words back in place of their translations, takes away
    * the pieces shown elsewhere, and joins each text of the page back together.
    */
  def restore(): Unit =
    document
      .querySelectorAll(s".$inserted")
      .foreach(element => element.parentNode.removeChild(element))
    document
      .querySelectorAll(s".$translated, .$gap")
      .foreach(element =>
        element.parentNode.replaceChild(restored(element), element),
      )
    join()

  /** The text that `element` was put in place of, from the same text. */
  private def restored(element: Element): Text =
    val text = document.createTextNode(
      Option(element.getAttribute(original)).getOrElse(""),
    )
    originOf(element).foreach(derive(text, _))
    text

  /**
    * Joins the texts split from each text of the page, or put back in place of
    * part of it, back into it, so that the page's texts are as they were.
    */
  private def join(): Unit = walk(document.body)
    .flatMap(text => originOf(text).map(_ -> text))
    .groupMap(_._1)(_._2)
    .foreach((origin, texts) => runs(texts).foreach(merge(_, origin)))

  /**
    * `texts`, in runs of those side by side in the page, as the page may have
    * put something between them since they were split.
    */
  private def runs(texts: List[Text]): List[List[Text]] =
    texts.foldRight(List.empty[List[Text]]):
      case (text, (next :: run) :: rest) if text.nextSibling == next =>
        (text :: next :: run) :: rest
      case (text, done) => List(text) :: done

  /**
    * Joins `run` into `origin`, the text of the page that it came from, if it
    * holds it, or else into its first text.
    */
  private def merge(run: List[Text], origin: Text): Unit =
    val into = if run.contains(origin) then origin else run.head
    if run.size > 1 then
      into.data = run.map(_.data).mkString
      run.filter(_ != into).foreach(text => text.parentNode.removeChild(text))

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
    * The parts of `node`: itself, if a text or not prose, or else its
    * children's.
    */
  private def partsOf(node: Node): List[Node] = node match
    case text: Text                                   => List(text)
    case element: Element if element.matches(skipped) => List(element)
    case _ => node.childNodes.toList.flatMap(partsOf)

  /**
    * What `part` said in the page before any of it was translated: its text, or
    * the original of a translation or gap, nothing for a piece shown away from
    * its word, or a space for anything else that is not prose.
    */
  private def said(part: Node): String = part match
    case text: Text => text.data
    case element: Element if element.matches(s".$translated, .$gap") =>
      Option(element.getAttribute(original)).getOrElse("")
    case element: Element if element.matches(s".$inserted") => ""
    case _                                                  => " "

  /**
    * A passage, as its parts in order: its texts of prose, and the elements in
    * it that are not prose, being ours or not to be translated. In `text`, each
    * such element is one [[placeholder]], so that the text on either side of it
    * is not taken as side by side.
    */
  private final class Passage(root: Node):

    private val parts = partsOf(root).toVector

    /** Where each part begins in `text`, then where the last ends. */
    private val starts = parts.scanLeft(0)((start, part) =>
      start +
        (part match
          case text: Text => text.length
          case _          => 1),
    )

    /** The texts among the parts, each with where it begins in `text`. */
    private val texts = parts
      .zip(starts)
      .collect { case (text: Text, start) => text -> start }

    val text: String = parts
      .map:
        case text: Text => text.data
        case _          => placeholder.toString
      .mkString

    /** The span of `node` in `text`, from `from` up to `to` within it. */
    def spanOf(node: Text, from: Int, to: Int): Option[Span] = texts
      .collectFirst { case (`node`, start) => Span(start + from, start + to) }

    /**
      * The text that `span` of `text` is within, if it is within one, with
      * where `span` begins and ends in it.
      */
    def within(span: Span): Option[(Text, Int, Int)] =
      for
        (start, from) <- at(span.start, false)
        (end, to)     <- at(span.end, true) if start == end
      yield (start, from, to)

    /**
      * The text and the place in it that `offset` in `text` is, at the end of
      * the text before it if `after`, or else at the start of the text after
      * it.
      */
    def at(offset: Int, after: Boolean): Option[(Text, Int)] = texts
      .find((node, start) =>
        if after then start < offset && offset <= start + node.length
        else start <= offset && offset < start + node.length,
      )
      .map((node, start) => node -> (offset - start))

    /**
      * The text of the passage before `span` of `node`, and after it, as the
      * page said it before any of it was translated, without what is not prose.
      */
    def around(node: Text, span: Span): (String, String) =
      val index = parts.indexOf(node)
      (
        parts.take(index).map(said).mkString + node.data.take(span.start),
        node.data.drop(span.end) + parts.drop(index + 1).map(said).mkString,
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
