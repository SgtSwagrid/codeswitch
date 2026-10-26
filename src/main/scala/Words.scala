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
  * marked as `[[word]]`, and each side cut short ends in `…`.
  */
final case class Word(text: String, context: String) derives Codec

/** Finds the words of a text, and chooses which of them to translate. */
object Words:

  /** The share of the words to translate. */
  val share = 0.05

  /** How much of the text on each side of a word to give as its context. */
  private val reach = 100

  /**
    * How much of the text on each side of a word to give as its context, at
    * most, to take in the rest of its sentence.
    */
  private val limit = 300

  /** A run of letters, with any apostrophes or hyphens within it. */
  private val pattern = """\p{L}[\p{L}\p{M}]*(?:['’-]\p{L}[\p{L}\p{M}]*)*""".r

  private val sentencer = Segmenter(
    js.undefined,
    js.Dictionary("granularity" -> "sentence"),
  )

  /** The words of `text`, in order. */
  def find(text: String): List[Span] = spans(pattern, text)

  /**
    * The places in `text` of `quote`, a word or words, as whole words, in any
    * case, and with any spacing between its words.
    */
  def occurrences(text: String, quote: String): List[Span] =
    val words = quote.trim.split("\\s+").toList.filter(_.nonEmpty)
    if words.isEmpty then Nil
    else
      spans(
        whole(words.map(Pattern.quote).mkString("\\s+")),
        text,
      )

  /** `pattern`, in any case, but only where no letter is beside it. */
  private def whole(pattern: String): Regex =
    s"(?iu)(?<![\\p{L}\\p{M}])$pattern(?![\\p{L}\\p{M}])".r

  private def spans(pattern: Regex, text: String): List[Span] = pattern
    .findAllMatchIn(text)
    .map(found => Span(found.start, found.end))
    .toList

  /** The sentences of `text`, in order. */
  def sentences(text: String): List[Span] = sentencer
    .segment(text)
    .toList
    .map(found =>
      Span(
        found.index,
        found.index + found.segment.length,
      ),
    )

  /** The sentence of `text` that the character `at` is in. */
  def sentenceAt(text: String, at: Int): Span = sentences(text)
    .find(sentence => sentence.start <= at && at < sentence.end)
    .getOrElse(Span(0, text.length))

  /**
    * Each of `words` longer than one letter, with a chance of [[share]],
    * drawing each chance from `random`.
    */
  def choose(words: List[Span], random: () => Double): List[Span] = words
    .filter(word => word.length > 1 && random() < share)

  /**
    * The context of `word`, between the texts `before` and `after` it once
    * their spacing is collapsed: as far as [[reach]] goes on each side, or to
    * the ends of the word's sentence within [[limit]], so that a word that
    * moves can be seen to. Each side cut short ends in `…`.
    */
  def context
    (
      before: String,
      word: String,
      after: String,
    )
    : String =
    val (prior, next) = (squash(before), squash(after))
    val near          = prior.takeRight(limit)
    val sentence      = sentenceAt(
      near + word + next.take(limit),
      near.length,
    )
    val left  = (near.length - sentence.start).max(reach)
    val right = (sentence.end - near.length - word.length).max(reach)
    s"${ cut(prior, left) }${ prior.takeRight(left) }[[$word]]${ next.take(
        right,
      ) }${ cut(next, right) }".trim

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

  def segment(text: String): js.Iterable[Segment] = js.native

/** A `segment` of a text, which begins at `index`. */
@js.native
private trait Segment extends js.Object:

  val segment: String = js.native

  val index: Int = js.native
