package com.alecdorrington.codeswitch

import com.alecdorrington.annotatedtextformat.{Annotation, TextSpan}
import com.alecdorrington.annotatedtextformat.llm.QuotedAnnotation
import com.alecdorrington.megalingual.Language
import io.circe.Codec
import java.util.regex.Pattern
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.util.matching.Regex

/**
  * A word to translate `into` a language being learnt, with its `context`: the
  * text around it, in which it is marked as `[[word]]`, and `…` marks where
  * either side was cut short, and how many of the page's words it stands for,
  * `times`, in counting how often its translation is met (see
  * [[FrequencyService]]): as one of a share of them drawn at random, one over
  * its chance of being drawn, so that the words drawn count for all; or none,
  * if it was drawn as the intensity rose, as its text's words were counted when
  * first drawn.
  */
final case class Word
  (
    text: String,
    context: String,
    into: Language,
    times: Double = 0,
  )
  derives Codec

/**
  * The word at `span`, with its `draw`, from `0` to `1`: it is translated while
  * the intensity is above its draw.
  */
final case class Drawn(span: TextSpan, draw: Double)

/** Finds the words of a text, and chooses which of them to translate. */
object Words:

  /** The share of the words to translate until the reader chooses another. */
  val defaultIntensity = 0.05

  /** The greatest share of the words that the reader may choose to translate. */
  val maxIntensity = 0.4

  /**
    * `intensity` kept from `0` up to [[maxIntensity]], as one kept by another
    * version of the plugin, or another device, may be out of that range.
    */
  def bounded(intensity: Double): Double = intensity.max(0).min(maxIntensity)

  /** How much of the text on each side of a word to give as its context. */
  private val reach = 100

  /**
    * How much of the text on each side of a word to give as its context, at
    * most, to take in the rest of its sentence.
    */
  private val limit = 300

  /** A run of letters, with any apostrophes or hyphens within it. */
  private val pattern = """\p{L}[\p{L}\p{M}]*(?:['’-]\p{L}[\p{L}\p{M}]*)*""".r

  /**
    * Splits texts into sentences, where the browser can: Firefox only could
    * from version 125.
    */
  private val sentencer =
    Option.when(!js.isUndefined(js.Dynamic.global.Intl.Segmenter))(Segmenter(
      js.undefined,
      js.Dictionary("granularity" -> "sentence"),
    ))

  /**
    * The end of a sentence, where the browser cannot find them: its stops, any
    * closing quotes or brackets, then a space.
    */
  private val stop = """[.!?。！？]+['"’”)\]]*\s+""".r

  /** The words of `text`, in order. */
  def find(text: String): List[TextSpan] = spans(pattern, text)

  /**
    * The places in `text` of `quote`, a word or words, as whole words, in any
    * case, with any spacing between its words, and either apostrophe for
    * either.
    */
  def occurrences(text: String, quote: String): List[TextSpan] =
    val words = quote.trim.split("\\s+").toList.filter(_.nonEmpty)
    if words.isEmpty then Nil
    else
      spans(
        whole(words.map(literal).mkString("\\s+")),
        text,
      )

  /** `word`, as a pattern, in which each apostrophe stands for either. */
  private def literal(word: String): String = word
    .split("['’]", -1)
    .map(Pattern.quote)
    .mkString("['’]")

  /**
    * `pattern`, in any case, but only where it is not part of a longer word, as
    * one joined to it by an apostrophe or hyphen.
    */
  private def whole(pattern: String): Regex =
    s"(?iu)(?<![\\p{L}\\p{M}]|\\p{L}['’-])$pattern(?![\\p{L}\\p{M}]|['’-]\\p{L})"
      .r

  private def spans(pattern: Regex, text: String): List[TextSpan] = pattern
    .findAllMatchIn(text)
    .map(found => TextSpan(found.start, found.end))
    .toList

  /**
    * The sentence of `text` that the character `at` is in. A line break is read
    * as a space, as a page shows one within its prose.
    */
  def sentenceAt(text: String, at: Int): TextSpan =
    val prose =
      text.map(char => if char == '\n' || char == '\r' then ' ' else char)
    sentencer
      .fold(byStops(prose).find(_.contains(at)))(
        _.segment(prose)
          .containing(at)
          .toOption
          .map(found =>
            TextSpan(
              found.index,
              found.index + found.segment.length,
            ),
          ),
      )
      .getOrElse(TextSpan(0, text.length))

  /** The sentences of `text`, in order, as [[sentenceAt]] finds them. */
  def sentences(text: String): List[TextSpan] =
    val prose =
      text.map(char => if char == '\n' || char == '\r' then ' ' else char)
    sentencer
      .fold(byStops(prose))(segmenter =>
        all(segmenter.segment(prose)).map(found =>
          TextSpan(
            found.index,
            found.index + found.segment.length,
          ),
        ),
      )
      .filter(_.length > 0)

  /** Every segment of `segments`, in order. */
  private def all(segments: Segments): List[Segment] =
    val each = segments.asInstanceOf[js.Iterable[Segment]].jsIterator()
    Iterator.continually(each.next()).takeWhile(!_.done).map(_.value).toList

  /**
    * `text` cut into parts of whole sentences, each of at most about `size`
    * words, but for a sentence longer than that, which is cut between its
    * words.
    */
  def excerpts(text: String, size: Int): List[TextSpan] = weighed(
    sentences(text).flatMap(cut(text, _, size)),
    size,
  )(span => find(span.in(text)).size).map(spans =>
    TextSpan(spans.head.start, spans.last.end),
  )

  /**
    * `sentence` of `text`, cut between its words into parts of at most `size`
    * words each.
    */
  private def cut
    (
      text: String,
      sentence: TextSpan,
      size: Int,
    )
    : List[TextSpan] =
    val starts = find(sentence.in(text))
      .grouped(size)
      .map(sentence.start + _.head.start)
      .toList
    if starts.size <= 1 then List(sentence)
    else
      (sentence.start :: starts.tail)
        .zip(starts.tail :+ sentence.end)
        .map(TextSpan(_, _))

  /** The sentences of `text`, each ending after its stop, as [[stop]] finds. */
  private[codeswitch] def byStops(text: String): List[TextSpan] =
    (List(0) ++ stop.findAllMatchIn(text).map(_.end) ++ List(text.length))
      .distinct
      .sliding(2)
      .collect { case List(start, end) => TextSpan(start, end) }
      .toList

  /**
    * Each of `words` longer than one letter whose draw is below `intensity`,
    * the share of the words to translate. Each word is drawn a number from `0`
    * to `1`, and translated while the intensity is above it. Words already
    * drawn up to `floor`, and not chosen, are drawn from `floor` to `1`, so
    * that raising the intensity keeps the words already chosen. Each draw is
    * from `random`.
    */
  def choose
    (
      words: List[TextSpan],
      floor: Double,
      intensity: Double,
      random: () => Double,
    )
    : List[Drawn] = words
    .filter(_.length > 1)
    .map(word => Drawn(word, floor + (1 - floor) * random()))
    .filter(_.draw < intensity)

  /**
    * The context of `word`, between the texts `before` and `after` it once
    * their spacing is collapsed: as far as [[reach]] goes on each side, or to
    * the ends of the word's sentence within [[limit]], so that a word that
    * moves can be seen to. `…` marks where either side is cut short.
    */
  def context
    (
      before: String,
      word: String,
      after: String,
    )
    : String =
    val (prior, next)  = (squash(before), squash(after))
    val (left, right)  = kept(prior, word, next)
    val (start, close) = (cut(prior, left), cut(next, right))
    s"$start${ prior.takeRight(left) }[[$word]]${ next.take(right) }$close".trim

  /**
    * How much of `prior` and of `next` to keep around `word`: [[reach]], or as
    * far as the ends of its sentence within [[limit]].
    */
  private def kept(prior: String, word: String, next: String): (Int, Int) =
    val near     = prior.takeRight(limit)
    val sentence = sentenceAt(
      near + word + next.take(limit),
      near.length,
    )
    (
      (near.length - sentence.start).max(reach),
      (sentence.end - near.length - word.length).max(reach),
    )

  /** `text` with each run of spacing collapsed into one space. */
  def squash(text: String): String = text.replaceAll("\\s+", " ")

  /** An `…`, if `text` is longer than the `kept` part of it. */
  private def cut(text: String, kept: Int): String =
    if text.length > kept then "…" else ""

  /**
    * `translation` beginning with a capital if `original` does, as at the start
    * of a sentence.
    */
  def matchCase(original: String, translation: String): String =
    if original.headOption.exists(_.isUpper) then translation.capitalize
    else translation

  /**
    * `items` packed as [[weighed]] packs them, nearest first by `distance`:
    * those in sight, at `0`, into batches of about `near`, then the rest into
    * batches of about `far`, so that what the reader sees is asked for in small
    * batches, which are answered sooner. Each batch keeps the order of `items`.
    */
  def nearestFirst[A]
    (items: List[A], near: Int, far: Int)
    (weight: A => Int, distance: A => Double)
    : List[List[A]] =
    val (seen, unseen) = items
      .zipWithIndex
      .map((item, index) => (item, index, distance(item)))
      .partition(_._3 == 0)
    val weighing = (each: (A, Int, Double)) => weight(each._1)
    (weighed(seen, near)(weighing) ++
      weighed(unseen.sortBy(_._3), far)(weighing)).map(_.sortBy(_._2).map(_._1))

  /**
    * `text` cut into pieces of whole sentences, each of at most `size`
    * characters, but for a sentence longer than that, which is cut every `size`
    * characters, so that no piece is too long to translate in one reply,
    * however few words a language's spacing makes of it.
    */
  def pieces(text: String, size: Int): List[TextSpan] = weighed(
    sentences(text).flatMap(sentence =>
      (sentence.start until sentence.end by size)
        .map(start => TextSpan(start, (start + size).min(sentence.end)))
        .toList,
    ),
    size,
  )(_.length).map(spans => TextSpan(spans.head.start, spans.last.end))

  /** What may end a word, but never begins one, as a comma or a stop. */
  private val closing = ",.;:!?)]}%»”’…"

  /** What may begin a word, but never ends one, as a bracket. */
  private val opening = "([{«“‘¿¡"

  /**
    * `next`, the translation of a part that follows `previous`, as shown, with
    * `gap` between them in the page, spaced off it if the page has no space
    * between them, as a language without spaces has none, but the translation's
    * language puts one there: neither ends nor begins in a space, `previous`
    * does not end opening something, and `next` does not begin by closing.
    */
  def spacedAfter
    (
      previous: String,
      gap: String,
      next: String,
    )
    : String =
    val apart = gap.exists(_.isWhitespace) || previous.isEmpty ||
      next.isEmpty || previous.last.isWhitespace || next.head.isWhitespace ||
      opening.contains(previous.last) || closing.contains(next.head)
    if apart then next else s" $next"

  /**
    * `items` packed in order into batches whose weights come to at most `size`,
    * but for an item heavier than `size`, which is a batch of its own.
    */
  def weighed[A](items: List[A], size: Int)(weight: A => Int): List[List[A]] =
    items
      .foldLeft(List.empty[(Int, List[A])]):
        case ((total, last) :: done, item) if total + weight(item) <= size =>
          (total + weight(item), last :+ item) :: done
        case (done, item) => (weight(item), List(item)) :: done
      .reverse
      .map(_._2)

  /**
    * Where each of `hints` is in `text`, a span for each part of a phrase split
    * by other words, with how it is marked there. Each is found as whole words,
    * as Annotated Text Format's anchoring finds a quote: in the order given,
    * from where the one before begins on, as written before in any case or
    * spacing, or else wherever its words are only once. One that is not found,
    * or is found only at a point, or would overlap one before it, is left out.
    */
  def place(text: String, hints: List[Hint]): List[Annotation[Marking]] =
    QuotedAnnotation
      .anchor(
        text,
        hints.map(hint => QuotedAnnotation(quoted(hint.original), hint)),
      )
      .annotated
      .annotations
      .filter(_.points.isEmpty)
      .foldLeft(List.empty[Annotation[Marking]])((kept, annotation) =>
        if kept.exists(_.overlaps(annotation)) then kept
        else kept :+ annotation.map(marking(text, annotation.spans, _)),
      )

  /**
    * `original` as Annotated Text Format quotes a phrase split by other words,
    * its parts joined by an ellipsis with a space either side.
    */
  private def quoted(original: String): String =
    parts(original).mkString(QuotedAnnotation.separator)

  /** What separates the parts of a phrase split by other words. */
  private val gap = """…|\.\.\.""".r

  /** The parts of `original`, a phrase that may be split by other words. */
  private def parts(original: String): List[String] = gap
    .split(original)
    .map(_.trim)
    .filter(_.nonEmpty)
    .toList

  /**
    * How the words at `spans` of `text`, which `hint` marks, are shown: as
    * their gloss, with them on hovering, if they are glossed, but for the parts
    * of a phrase split by other words, which are explained; or else as they
    * are, with the meaning on hovering.
    */
  private def marking
    (
      text: String,
      spans: List[TextSpan],
      hint: Hint,
    )
    : Marking = spans match
    case List(span) if hint.kind == Hint.Kind.Glossed =>
      Marking(
        Some(
          if opens(text, span.start) then hint.gloss.capitalize else hint.gloss,
        ),
        span.in(text),
        Hint.Kind.Glossed,
        hint,
      )
    case _ if hint.kind == Hint.Kind.Glossed =>
      Marking(
        None,
        hint.meaning,
        Hint.Kind.Explained,
        hint,
      )
    case _ => Marking(None, hint.meaning, hint.kind, hint)

  /** What may stand between the start of a sentence and its first word. */
  private val openers = "\"'“”„«»‹›‘’‚(["

  /**
    * Whether the word at `at` begins its sentence of `text`, as a German noun's
    * capital is not its meaning's.
    */
  def opens(text: String, at: Int): Boolean = text
    .substring(sentenceAt(text, at).start, at)
    .forall(char => char.isWhitespace || openers.contains(char))

/** Splits texts into segments, as sentences, by the rules of the locale. */
@js.native
@JSGlobal("Intl.Segmenter")
private class Segmenter
  (
    locales: js.UndefOr[String],
    options: js.Dictionary[String],
  )
  extends js.Object:

  def segment(text: String): Segments = js.native

/** The segments of a text. */
@js.native
private trait Segments extends js.Object:

  /** The segment that the character at `index` is in, if any. */
  def containing(index: Int): js.UndefOr[Segment] = js.native

/** A `segment` of a text, which begins at `index`. */
@js.native
private trait Segment extends js.Object:

  val segment: String = js.native

  val index: Int = js.native
