package com.alecdorrington.codeswitch

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import com.alecdorrington.annotatedtextformat.{
  AnnotatedText, Annotation, TextSpan,
}
import com.alecdorrington.annotatedtextformat.dom.{
  Colour, Decoration, Decorator, Hovering, Look, Passage, Refusal, Style,
}
import com.alecdorrington.megalingual.Language
import fs2.Stream
import org.scalajs.dom.*
import scala.collection.mutable
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

/** That a passage was found in `pass` to be in `language`, if in one known. */
private final case class Known(pass: Int, language: Option[Language])

/**
  * A part of a passage of the page in `learnt`, a language being learnt, held
  * by `block`, at `range`, which keeps to it as the page changes around it, of
  * which `text` is what is asked about, with its number of `words`.
  */
final case class Excerpt
  (
    learnt: Learnt,
    block: Node,
    range: Range,
    text: String,
    words: Int,
  )

/**
  * A passage of the page in a foreign language, or a stretch of one too long to
  * translate at once, held by `block`, at `range`, which keeps to it as the
  * page changes around it, of the passage's own text, `original`, in `parts`,
  * each translated in its place, of `size` characters in all, into a language
  * `spaced` or not.
  */
final case class Rendering
  (
    block: Node,
    range: Range,
    original: String,
    parts: List[Part],
    size: Int,
    spaced: Boolean,
  )

/**
  * A part of a passage translated whole: the text of one of its text nodes, at
  * `range`, which keeps to it, as `text` is sent.
  */
final case class Part(range: Range, text: String)

/**
  * A part of a passage translated whole as it is put in the page: what it
  * shows, and the `original` passage, shown on hovering over any part of it.
  */
final case class Rendered(shown: String, original: String)

/**
  * A translation as it is put in the page: what each of its parts shows, in
  * order, the `original` text that it stands for, the `draw` of its word, and
  * the translation's dictionary form, `word`, of `language`, the one being
  * learnt that it is in, and part of speech, `pos`, if known.
  */
final case class Translated
  (
    shown: List[String],
    original: String,
    draw: Double,
    word: String,
    language: Language,
    pos: String,
  ):

  /**
    * The word that the card of the translation is of, which means its original,
    * rated as explained, as a translation has no level.
    */
  def headword: Headword = Headword(
    word,
    language,
    pos,
    original,
    original,
    Hint.Kind.Explained,
  )

/**
  * A translation laid out in the page, as `annotation`, in the link, button or
  * label `label`, if any, of whose letters it takes away `emptied`.
  */
private final case class Laid
  (
    annotation: Annotation[Translated],
    label: Option[Element],
    emptied: Int,
  )

/**
  * Finds the words of the page to translate, and shows the pieces of their
  * translations, each highlighted, where the language's word order puts it,
  * with the card of its word shown by [[Tips]] on hovering over it. A
  * translation whose draw is above the intensity is hidden, and reads as the
  * original text, until the intensity rises above it. Annotated Text Format
  * puts each into the page, and takes it out again, leaving the page's text as
  * it was.
  */
object Highlighter:

  /** The class of each translation, or piece of one, in the page. */
  private val translated = "codeswitch"

  /** The class of each hint in the page. */
  private val hinted = "codeswitch-hint"

  /** The class of each hint shown as its meaning, in the reader's language. */
  private val glossed = "codeswitch-gloss"

  /** The class of each part of a passage translated whole. */
  private val rendered = "codeswitch-rendered"

  /** The class of each hint that the reader knows, only faintly marked. */
  private val known = "codeswitch-known"

  /** Text that is not prose, or not to be translated, or ours. */
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
    s"[${ Passage.decorated }]",
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

  /** The colour of each translation, and of each hint explained. */
  private val teal = Colour("#14b8a6")

  /** The colour of each hint glossed. */
  private val grey = Colour("#94a3b8")

  /** The underline of a hint that the reader knows, thin and faint. */
  private val faint = Style.Layer(
    Style.fill(Colour("rgb(20 184 166 / 50%)")),
    "100% 1px",
    "0 100%",
  )

  /** How each translation looks: each part showing its piece. */
  private val translationLook = Look[Translated](
    _ => teal,
    (translated, part) => translated.shown.lift(part),
  )

  /**
    * How each part of a passage translated whole looks: as its translation, not
    * highlighted, as the whole passage would be, but faintly underlined.
    */
  private val renderLook = Look[Rendered](
    _ => Colour("transparent"),
    (rendered, _) => Some(rendered.shown),
    _ =>
      List(Style.Layer(
        Style.fill(Colour("rgb(148 163 184 / 60%)")),
        "100% 1px",
        "0 100%",
      )),
  )

  /**
    * How each hint looks: as it is, or as its meaning, highlighted as its kind
    * says, or, if the reader knows it, only faintly underlined.
    */
  private val hintLook = Look[Marking](
    marking =>
      marking.kind match
        case Hint.Kind.Explained => teal
        case Hint.Kind.Glossed   => grey
        case Hint.Kind.Known     => Colour("transparent"),
    (marking, _) => marking.gloss,
    markings => markings.find(_.kind == Hint.Kind.Known).map(_ => faint).toList,
    marking =>
      marking.kind match
        case Hint.Kind.Explained => Nil
        case Hint.Kind.Glossed   => List(glossed)
        case Hint.Kind.Known     => List(known),
  )

  /**
    * The translations put in the page in this pass, each hidden while the
    * intensity is below its draw.
    */
  private val translationsShown = mutable
    .ArrayBuffer
    .empty[Decoration[Translated]]

  /** The hints put in the page in this pass. */
  private val hintsShown = mutable.ArrayBuffer.empty[Decoration[Marking]]

  /** The passages translated whole in this pass. */
  private val renderingsShown = mutable.ArrayBuffer.empty[Decoration[Rendered]]

  /**
    * The words to translate at `intensity` in the texts beneath `roots` whose
    * words have not been drawn up to it in `pass`, after which they have, of
    * the passages whose words `learner` switches (see [[Learner.switches]]), in
    * the order of the page, each into a language being learnt, drawn for it.
    */
  def choose
    (
      roots: List[Node],
      pass: Int,
      intensity: Double,
      learner: Learner,
    )
    : List[Choice] = texts(roots)
    .filter(node => learner.switchesFound(foundIn(passageOf(node), pass)))
    .map(node => node -> level(node, pass))
    .filter((_, floor) => floor < intensity)
    .flatMap((node, floor) =>
      stamp(node, Mark(pass, intensity))
      choices(node, floor, intensity, learner),
    )

  /** The texts beneath `roots`, or among them, that may be marked. */
  private def texts(roots: List[Node]): List[Text] = roots
    .flatMap(walk(_).filter(readable))
    .distinct

  /** The elements holding the passages of the texts beneath `roots`. */
  private def blocks(roots: List[Node]): List[Node] = texts(roots)
    .map(passageOf)
    .distinct

  /**
    * The passages beneath `roots` whose languages are not yet known in `pass`,
    * each with its own text, without that of the passages within it.
    */
  def unknown(roots: List[Node], pass: Int): List[(Node, String)] =
    blocks(roots)
      .filter(block => knownOf(block).forall(_.pass != pass))
      .map(block => block -> Words.squash(prose(own(block))).trim)

  /**
    * How many words of the page's passages were found in each language in the
    * pass, so that one found in none takes the page's.
    */
  private var tally = Tally.empty

  /**
    * That the passage of `block`, of `text`, is in `language`, as found in
    * `pass`, its words counted towards the page's language.
    */
  def learn
    (
      block: Node,
      pass: Int,
      language: Option[Language],
      text: String,
    )
    : Unit =
    language.foreach(language =>
      tally = tally.add(pass, language, Words.find(text).size),
    )
    block.asInstanceOf[js.Dynamic].codeswitchLanguage = Known(pass, language)
      .asInstanceOf[js.Any]

  /**
    * That each passage beneath `roots` found in `pass` to be in no language is
    * in the page's own, the language of most of its words found in the pass, if
    * any were: as headings and captions, too short to tell, are.
    */
  def prevail(roots: List[Node], pass: Int): Unit = tally
    .prevailing(pass)
    .foreach(language =>
      blocks(roots)
        .filter(block => knownOf(block).contains(Known(pass, None)))
        .foreach(block =>
          block.asInstanceOf[js.Dynamic].codeswitchLanguage =
            Known(pass, Some(language)).asInstanceOf[js.Any],
        ),
    )

  /**
    * The language that the page says the passage of `block` is in, by the
    * `lang` of the nearest element around it that has one, if it is known.
    */
  def declared(block: Node): Option[Language] = Option(block)
    .collect { case element: Element => element.closest("[lang]") }
    .filter(_ != null)
    .flatMap(element => Language.fromTag(element.getAttribute("lang")))

  private def knownOf(block: Node): Option[Known] = block
    .asInstanceOf[js.Dynamic]
    .codeswitchLanguage
    .asInstanceOf[js.UndefOr[Known]]
    .toOption

  /**
    * The language of the passage of `block`, if it is known, as found in
    * `pass`.
    */
  private def languageOf(block: Node, pass: Int): Option[Language] =
    foundIn(block, pass).flatten

  /**
    * What was found of the language of the passage of `block` in `pass`, if it
    * has been looked at: the language, if it is in one known.
    */
  private def foundIn(block: Node, pass: Int): Option[Option[Language]] =
    knownOf(block).filter(_.pass == pass).map(_.language)

  /**
    * The passages beneath `roots` in a language that `learner` is learning and
    * reads for its hard words (see [[Learnt.reads]]), not yet read in `pass`,
    * after which they have been, each with its own text, without that of the
    * passages within it.
    */
  def unread
    (
      roots: List[Node],
      pass: Int,
      learner: Learner,
      size: Int,
    )
    : List[Excerpt] = blocks(roots)
    .filterNot(readIn(_, pass))
    .flatMap(block =>
      languageOf(block, pass)
        .flatMap(learner.learnt)
        .filter(_.reads)
        .toList
        .flatMap(learnt =>
          block.asInstanceOf[js.Dynamic].codeswitchRead = pass
          excerpts(learnt, block, size),
        ),
    )
    .filter(_.words > 0)

  /**
    * The parts of the passage of `block`, in `learnt`, without the passages
    * within it, each of whole sentences, and of at most about `size` words, so
    * that the hints in each fit in one reply.
    */
  private def excerpts(learnt: Learnt, block: Node, size: Int): List[Excerpt] =
    val passage = own(block)
    val text    = prose(passage)
    Words
      .excerpts(text, size)
      .flatMap(span =>
        val excerpt = Words.squash(span.in(text)).trim
        passage
          .rangeOf(span)
          .map(Excerpt(
            learnt,
            block,
            _,
            excerpt,
            Words.find(excerpt).size,
          )),
      )

  /**
    * The passages beneath `roots` in a foreign language that `learner` has
    * translated whole (see [[Learner.translates]]), not yet read in `pass`,
    * after which they have been, each in the parts of its own text, without
    * that of the passages within it, in stretches of at most about `size`
    * characters.
    */
  def foreign
    (
      roots: List[Node],
      pass: Int,
      learner: Learner,
      size: Int,
    )
    : List[Rendering] = blocks(roots)
    .filterNot(readIn(_, pass))
    .filter(block => learner.translates(languageOf(block, pass)))
    .flatMap(block =>
      block.asInstanceOf[js.Dynamic].codeswitchRead = pass
      rendering(block, learner.native.spaced, size),
    )

  /**
    * The passage of `block`, in the parts of its own text, each of a text
    * node's text, cut between sentences into pieces of at most `size`
    * characters, in stretches of at most about `size` characters, translated
    * into a language `spaced` or not.
    */
  private def rendering
    (block: Node, spaced: Boolean, size: Int)
    : List[Rendering] =
    val passage = own(block)
    val whole   = document.createRange()
    whole.selectNodeContents(block)
    val original = Words.squash(prose(passage)).trim
    Words
      .weighed(
        texts(List(block))
          .filter(passageOf(_) == block)
          .flatMap(partsOf(passage, _, spaced, size)),
        size,
      )(_.text.length)
      .map(parts =>
        Rendering(
          block,
          whole,
          original,
          parts,
          parts.map(_.text.length).sum,
          spaced,
        ),
      )

  /**
    * The parts of `passage` that `node`, one of its text nodes, holds, if it
    * holds anything but spacing, each of at most `size` characters, of whole
    * sentences where it can be. Into a language `spaced`, the spacing at its
    * ends is left in the page; into one not, it is taken in, so that the page's
    * spaces between parts are not left between words that have none.
    */
  private def partsOf
    (
      passage: Passage,
      node: Text,
      spaced: Boolean,
      size: Int,
    )
    : List[Part] = (
    passage.offsetOf(node, 0),
    passage.offsetOf(node, node.length),
  ).mapN(TextSpan(_, _))
    .map(span => if spaced then trimmed(passage.text, span) else span)
    .filter(span => span.in(passage.text).trim.nonEmpty)
    .toList
    .flatMap(span =>
      Words
        .pieces(span.in(passage.text), size)
        .map(_.shift(span.start))
        .map(piece => if spaced then trimmed(passage.text, piece) else piece)
        .filter(_.length > 0)
        .flatMap(piece =>
          passage
            .rangeOf(piece)
            .map(Part(
              _,
              Words.squash(piece.in(passage.text)),
            )),
        ),
    )

  /** `span` of `text` without the spacing at its ends. */
  private def trimmed(text: String, span: TextSpan): TextSpan =
    val within = span.in(text)
    TextSpan(
      span.start + within.length - within.stripLeading.length,
      span.end - (within.length - within.stripTrailing.length),
    )

  /**
    * Shows `translated`, the parts of `rendering` put into the reader's own
    * language, each in place of its part as the page is now, spaced off the
    * part before it where the page has no space between them but the reader's
    * language puts one (see [[Words.spacedAfter]]), and one given none as
    * nothing. A part left as it was is not touched, and nor is one that the
    * page has changed since it was read, which is left out, and the rest of the
    * page is still read. Those within links, buttons and the like show no
    * original, as a tap on one would first show it, not follow it; the others
    * show it covered by `tips`.
    */
  def render
    (
      rendering: Rendering,
      translated: List[String],
      tips: Tips,
    )
    : Unit = attempt:
    val passage = own(rendering.block)
    val placed  = rendering
      .parts
      .zip(translated)
      .flatMap((part, shown) =>
        Option
          .when(unchanged(part))(passage.spanOf(part.range))
          .flatten
          .map(span => (part, span, shown)),
      )
    val shown = placed
      .zip(None :: placed.map(Some(_)))
      .map { case ((part, span, shown), before) =>
        (
          part,
          before.fold(shown)((_, previous, said) =>
            if rendering.spaced then
              Words.spacedAfter(
                said,
                passage.text.substring(previous.end, span.start),
                shown,
              )
            else shown,
          ),
        )
      }
      .filterNot((part, shown) => shown == part.text)
    val (linked, free) = shown.partition((part, _) => labelled(part.range))
    renderIn(rendering, free, Some(tips))
    renderIn(rendering, linked, None)

  /**
    * Shows each of `shown`, a part of `rendering` with its translation, in its
    * place in the passage as it is now, covered by `tips`, if any.
    */
  private def renderIn
    (
      rendering: Rendering,
      shown: List[(Part, String)],
      tips: Option[Tips],
    )
    : Unit =
    val passage = own(rendering.block)
    decorate(
      passage,
      shown.flatMap((part, text) =>
        passage
          .spanOf(part.range)
          .map(span =>
            Annotation(
              List(span),
              Rendered(text, rendering.original),
            ),
          ),
      ),
      renderLook,
      rendered,
    ).foreach(decoration =>
      renderingsShown += decoration
      tips.foreach(_.renderings.add(decoration)),
    )

  /**
    * Whether `part` still holds what it held when it was read: within one text
    * of the page, which it has not left, nor rewritten.
    */
  private def unchanged(part: Part): Boolean =
    settle(part.range)
    part.range.startContainer match
      case node: Text => node == part.range.endContainer &&
        document.body.contains(node) &&
        Words.squash(part.range.toString) == part.text
      case _ => false

  /** Whether `range` is within a link, button or label. */
  private def labelled(range: Range): Boolean = range.startContainer match
    case node: Text => labelOf(node).isDefined
    case _          => false

  private def readIn(block: Node, pass: Int): Boolean = block
    .asInstanceOf[js.Dynamic]
    .codeswitchRead
    .asInstanceOf[js.UndefOr[Int]] == js.defined(pass)

  /**
    * The passage of `block`, in which the passages within it, and all that is
    * not to be translated, are not prose.
    */
  private def own(block: Node): Passage = Passage(
    block,
    element => element.matches(skipped) || element.matches(passages),
  )

  /**
    * The passage of `block`, in which all that is not to be translated is not
    * prose.
    */
  private def entire(block: Node): Passage = Passage(block, _.matches(skipped))

  /** The text of `passage`, with each part that is not prose as a space. */
  private def prose(passage: Passage): String = passage
    .text
    .replace(Passage.placeholder, ' ')

  /**
    * Shows `hints` in the passage of `excerpt`, each in its place in its text
    * as it is now, covered by `tips`. If the page has changed so that one
    * cannot be, it is left out, and the rest of the page is still read.
    */
  def hint
    (
      excerpt: Excerpt,
      hints: List[Hint],
      tips: Tips,
    )
    : Unit = attempt:
    settle(excerpt.range)
    val passage = own(excerpt.block)
    passage
      .spanOf(excerpt.range)
      .flatMap(region =>
        decorate(
          passage,
          Words
            .place(region.in(passage.text), hints)
            .map(shift(_, region.start)),
          hintLook,
          hinted,
        ),
      )
      .foreach(decoration =>
        hintsShown += decoration
        tips.hints.add(decoration),
      )

  /** `annotation`, moved `by` characters on. */
  private def shift[A](annotation: Annotation[A], by: Int): Annotation[A] =
    annotation.copy(spans = annotation.spans.map(_.shift(by)))

  /**
    * Those of `annotations` that fit the text of `passage`, put into it as
    * `look` shows them, each whole or not at all, with `prefix` as their class,
    * unless none can be. Each text split off another keeps its mark, so that
    * its words are not drawn again. What is left out, and why, is logged for
    * debugging.
    */
  private def decorate[A]
    (
      passage: Passage,
      annotations: List[Annotation[A]],
      look: Look[A],
      prefix: String,
    )
    : Option[Decoration[A]] = Option
    .when(annotations.nonEmpty)(Decorator.decorate(
      passage,
      AnnotatedText.fitting(passage.text, annotations),
      look,
      prefix,
      whole = true,
      split = (node, rest) => stamp(rest, markOf(node)),
    ))
    .flatMap(_.fold(refused, Some(_)))
    .flatMap(kept)

  /** Nothing, as `refusal` gives, which is logged for debugging. */
  private def refused(refusal: Refusal): Option[Nothing] =
    console.debug(s"Codeswitch: ${ refusal.message }")
    None

  /**
    * `decoration`, if any of its annotations are in the page, or else nothing,
    * once it is taken out again. How many could not be put in whole is logged
    * for debugging.
    */
  private def kept[A](decoration: Decoration[A]): Option[Decoration[A]] =
    val (left, all) =
      (decoration.unplaced.size, decoration.annotated.annotations.size)
    if left > 0 then
      console.debug(s"Codeswitch: $left of $all could not be shown whole.")
    if left < all then Some(decoration)
    else
      decoration.restore()
      None

  /** Runs `edit`, warning of why it failed, if it did. */
  private def attempt(edit: => Unit): Unit = Try(edit)
    .failed
    .foreach(error => console.warn(s"Codeswitch: ${ error.getMessage }"))

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
    * below `floor` already, each into a language that `learner` is learning,
    * drawn for it.
    */
  private def choices
    (
      node: Text,
      floor: Double,
      intensity: Double,
      learner: Learner,
    )
    : List[Choice] =
    lazy val block = passageOf(node)
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
          word(
            block,
            node,
            drawn.span,
            learner.into(math.random()),
          ),
        ),
      )

  /**
    * The word at `span` in `node`, with the text around it in the passage of
    * `block`, so that a word in a link or in emphasis has the sentence it is
    * part of. The text is the page's own, as it was before any of it was
    * translated, and without what is not to be translated, as code, scripts and
    * text fields.
    */
  private def word
    (
      block: Node,
      node: Text,
      span: TextSpan,
      into: Language,
    )
    : Word =
    val (before, after) = around(block, node, span)
    Word(
      span.in(node.data),
      Words.context(before, span.in(node.data), after),
      into,
    )

  /**
    * The text of the passage of `block` before `span` of `node`, and after it,
    * as the page said it before any of it was translated.
    */
  private def around
    (block: Node, node: Text, span: TextSpan)
    : (String, String) =
    val parts = block.childNodes.toList.flatMap(partsOf)
    val index = parts.indexOf(node)
    (
      parts.take(index).map(said).mkString + node.data.take(span.start),
      node.data.drop(span.end) + parts.drop(index + 1).map(said).mkString,
    )

  /**
    * The parts of `node`: itself, if a text or not prose, or else its
    * children's.
    */
  private def partsOf(node: Node): List[Node] = node match
    case text: Text                                   => List(text)
    case element: Element if element.matches(skipped) => List(element)
    case _ => node.childNodes.toList.flatMap(partsOf)

  /**
    * What `part` said in the page before any of it was translated: its text,
    * the text that a translation or hint took the place of, or a space for
    * anything else that is not prose.
    */
  private def said(part: Node): String = part match
    case text: Text => text.data
    case element: Element if element.hasAttribute(Passage.decorated) =>
      element.getAttribute(Passage.decorated)
    case _ => " "

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
    * or `0` if they have not been. Each change of the reader's choices begins a
    * new pass.
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
    * translation cannot all be shown, covered by `tips`. Those of a passage are
    * put in together, each passage apart from the rest, so that one that fails
    * leaves them be. Each is hidden unless its draw is below `intensity`.
    */
  def show
    (
      choices: List[Choice],
      translations: List[Translation],
      intensity: Double,
      tips: Tips,
    )
    : Unit = choices
    .zip(translations)
    .groupBy((choice, _) => wordOf(choice).map(passageOf))
    .collect { case (Some(block), words) => block -> words }
    .foreach((block, words) => attempt(showIn(block, words, intensity, tips)))

  /**
    * Shows each translation of `words`, which are in the order of the page,
    * around its word, in the passage of `block`, laid out by [[Placement]] in
    * its text as it is now, unless that would empty a link. Of two that would
    * overlap, the later is shown, as a noun is with its article, before the
    * article alone.
    */
  private def showIn
    (
      block: Node,
      words: List[(Choice, Translation)],
      intensity: Double,
      tips: Tips,
    )
    : Unit =
    val passage = entire(block)
    decorate(
      passage,
      sparing(
        words
          .reverse
          .flatMap((choice, translation) =>
            wordOf(choice).flatMap(laid(passage, _, choice, translation)),
          ),
      ),
      translationLook,
      translated,
    ).foreach(decoration =>
      reveal(decoration, intensity)
      translationsShown += decoration
      tips.translations.add(decoration),
    )

  /**
    * `translation` of the word of `choice`, in `node`, laid out by
    * [[Placement]] in the text of `passage`, holding the word's draw.
    */
  private def laid
    (
      passage: Passage,
      node: Text,
      choice: Choice,
      translation: Translation,
    )
    : Option[Laid] =
    def span(from: Int, to: Int): Option[TextSpan] =
      (passage.offsetOf(node, from), passage.offsetOf(node, to)).mapN(
        TextSpan(_, _),
      )
    for
      within <- span(0, node.length)
      word   <- span(
        choice.range.startOffset,
        choice.range.endOffset,
      )
      layout <- Placement.lay(passage.text, within, word, translation)
    yield Laid(
      Annotation(
        layout.parts.map(_._1),
        Translated(
          layout.parts.map(_._2),
          layout.home.in(passage.text),
          choice.draw,
          translation.word,
          choice.word.into,
          translation.pos,
        ),
      ),
      labelOf(node),
      if layout.shown.isEmpty then letters(layout.home.in(passage.text)) else 0,
    )

  /** The link, button or label that `node` is in, if any. */
  private def labelOf(node: Text): Option[Element] = Option(node.parentNode)
    .collect { case parent: Element => parent.closest(labels) }
    .filter(_ != null)

  /**
    * The annotations of `laid`, but those that would take away the last of the
    * letters of a link, button or label, with those before them, leaving
    * nothing in it to click or read.
    */
  private def sparing(laid: List[Laid]): List[Annotation[Translated]] = laid
    .foldLeft((Map.empty[Element, Int], Vector.empty[Annotation[Translated]])):
      case ((taken, kept), Laid(annotation, Some(label), emptied))
        if emptied > 0 =>
        val now = taken.getOrElse(label, 0) + emptied
        if now >= letters(label.textContent) then (taken, kept)
        else (taken.updated(label, now), kept :+ annotation)
      case ((taken, kept), each) => (taken, kept :+ each.annotation)
    ._2
    .toList

  /** The number of letters and digits in `text`. */
  private def letters(text: String): Int = text.count(_.isLetterOrDigit)

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
    * Shows each translation whose draw is below `intensity`, and hides the
    * rest, so that each reads as its original text.
    */
  def reveal(intensity: Double): Unit =
    translationsShown.foreach(reveal(_, intensity))

  /**
    * Shows each translation of `decoration` whose draw is below `intensity`,
    * and hides the rest.
    */
  private def reveal
    (
      decoration: Decoration[Translated],
      intensity: Double,
    )
    : Unit = decoration
    .annotated
    .annotations
    .zipWithIndex
    .filter((annotation, index) =>
      !decoration.unplaced(index) && (annotation.value.draw < intensity) !=
        decoration.isShown(index),
    )
    .foreach((annotation, index) =>
      if annotation.value.draw < intensity then decoration.show(index)
      else decoration.hide(index),
    )

  /**
    * Puts the original words back in place of the translations and hints, and
    * joins each text of the page back together, no longer covered by `tips`.
    */
  def restore(tips: Tips): Unit =
    tips.clear()
    translationsShown.foreach(_.restore())
    hintsShown.foreach(_.restore())
    renderingsShown.foreach(_.restore())
    translationsShown.clear()
    hintsShown.clear()
    renderingsShown.clear()
    sweep()

  /**
    * Puts back the text in place of each translation or hint still in the page,
    * which no decoration knows of, as one that the page copied, taking away
    * each piece put in.
    */
  private def sweep(): Unit = document
    .querySelectorAll(s"[${ Passage.decorated }][class^=$translated]")
    .foreach(element =>
      Option(element.parentNode).foreach(_.replaceChild(
        document.createTextNode(element.getAttribute(Passage.decorated)),
        element,
      )),
    )

  /**
    * Takes out of the page, and forgets, each translation and hint of which
    * nothing is in the page any more, as of a passage that the page took out,
    * no longer covered by `tips`, so that a page that keeps changing does not
    * leave them to pile up.
    */
  def prune(tips: Tips): Unit =
    forget(translationsShown, tips.translations)
    forget(hintsShown, tips.hints)
    forget(renderingsShown, tips.renderings)

  /**
    * Takes out of the page, and forgets, each of `made` of which nothing is in
    * the page, no longer covered by `hovering`.
    */
  private def forget[A]
    (
      made: mutable.ArrayBuffer[Decoration[A]],
      hovering: Hovering[A],
    )
    : Unit =
    val gone = made.filterNot(present).toList
    gone.foreach(decoration =>
      hovering.remove(decoration)
      decoration.restore(),
    )
    made --= gone

  /** Whether any element of `decoration` is in the page. */
  private def present(decoration: Decoration[?]): Boolean = decoration
    .annotated
    .annotations
    .indices
    .exists(decoration.elements(_).exists(_.isConnected))

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
    * How far `range` is from the window, in pixels across and down: `0` if any
    * of it is in sight, or infinite if it is not shown at all.
    */
  def distance(range: Range): Double =
    val box = range.getBoundingClientRect()
    if box.width == 0 && box.height == 0 then Double.PositiveInfinity
    else
      beyond(box.left, box.right, window.innerWidth) +
        beyond(box.top, box.bottom, window.innerHeight)

  /**
    * How far the span from `start` to `end` is outside the one from `0` to
    * `extent`.
    */
  private def beyond(start: Double, end: Double, extent: Double): Double =
    (start - extent).max(-end).max(0)

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
