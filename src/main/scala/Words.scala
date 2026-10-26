package com.alecdorrington.codeswitch

import com.alecdorrington.omnibrowser.Codec
import java.util.regex.Pattern
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.util.matching.Regex

/** A part of a text, as a word, from `start` up to but excluding `end`. */
final case class Span(start: Int, end: Int):

  /** The number of characters within it. */
  def length: Int = end - start

  /** Its part of `text`. */
  def in(text: String): String = text.substring(start, end)

  /** Whether the character `at` lies within it. */
  def contains(at: Int): Boolean = start <= at && at < end

  /** Whether `other` lies within it. */
  def covers(other: Span): Boolean = start <= other.start && other.end <= end

  /** Whether it shares a character with `other`. */
  def overlaps(other: Span): Boolean = start < other.end && other.start < end

  /** How many characters lie between it and `other`. */
  def distance(other: Span): Int = (other.start - end)
    .max(start - other.end)
    .max(0)

/**
  * A word to translate, with its `context`: the text around it, in which it is
  * marked as `[[word]]`, and `…` marks where either side was cut short.
  */
final case class Word(text: String, context: String) derives Codec

/**
  * The word at `span`, with its `draw`, from `0` to `1`: it is translated while
  * the intensity is above its draw.
  */
final case class Drawn(span: Span, draw: Double)

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
  def find(text: String): List[Span] = spans(pattern, text)

  /**
    * The places in `text` of `quote`, a word or words, as whole words, in any
    * case, with any spacing between its words, and either apostrophe for
    * either.
    */
  def occurrences(text: String, quote: String): List[Span] =
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

  private def spans(pattern: Regex, text: String): List[Span] = pattern
    .findAllMatchIn(text)
    .map(found => Span(found.start, found.end))
    .toList

  /**
    * The sentence of `text` that the character `at` is in. A line break is read
    * as a space, as a page shows one within its prose.
    */
  def sentenceAt(text: String, at: Int): Span =
    val prose =
      text.map(char => if char == '\n' || char == '\r' then ' ' else char)
    sentencer
      .fold(byStops(prose).find(_.contains(at)))(
        _.segment(prose)
          .containing(at)
          .toOption
          .map(found =>
            Span(
              found.index,
              found.index + found.segment.length,
            ),
          ),
      )
      .getOrElse(Span(0, text.length))

  /** The sentences of `text`, each ending after its stop, as [[stop]] finds. */
  private[codeswitch] def byStops(text: String): List[Span] =
    (List(0) ++ stop.findAllMatchIn(text).map(_.end) ++ List(text.length))
      .distinct
      .sliding(2)
      .collect { case List(start, end) => Span(start, end) }
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
      words: List[Span],
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

  private def squash(text: String): String = text.replaceAll("\\s+", " ")

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
    * `groups` packed in order into batches of about `size`, keeping each group
    * whole, so that a group larger than `size` is a batch of its own.
    */
  def batches[A](groups: List[List[A]], size: Int): List[List[A]] = groups
    .foldLeft(List.empty[List[A]]):
      case (last :: done, group) if last.size + group.size <= size =>
        (last ++ group) :: done
      case (done, group) => group :: done
    .reverse

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
