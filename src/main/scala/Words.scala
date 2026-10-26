package com.alecdorrington.codeswitch

import com.alecdorrington.omnibrowser.Codec

/** The place of a word in a text, from `start` up to but excluding `end`. */
final case class Span(start: Int, end: Int):

  /** The number of characters in the word. */
  def length: Int = end - start

  /** The word, in `text`. */
  def in(text: String): String = text.substring(start, end)

/**
  * A word to translate, with its `context`: the text around it, in which it is
  * marked as `[[word]]`.
  */
final case class Word(text: String, context: String) derives Codec

/** Finds the words of a text, and chooses which of them to translate. */
object Words:

  /** The share of the words to translate. */
  val share = 0.05

  /** How much of the text on each side of a word to give as its context. */
  private val reach = 100

  /** A run of letters, with any apostrophes or hyphens within it. */
  private val pattern = """\p{L}[\p{L}\p{M}]*(?:['’-]\p{L}[\p{L}\p{M}]*)*""".r

  /** The words of `text`, in order. */
  def find(text: String): List[Span] = pattern
    .findAllMatchIn(text)
    .map(found => Span(found.start, found.end))
    .toList

  /**
    * Each of `words` longer than one letter, with a chance of [[share]],
    * drawing each chance from `random`.
    */
  def choose(words: List[Span], random: () => Double): List[Span] = words
    .filter(word => word.length > 1 && random() < share)

  /**
    * The context of `word`, between the texts `before` and `after` it, as far
    * as [[reach]] goes on each side once their spacing is collapsed.
    */
  def context
    (
      before: String,
      word: String,
      after: String,
    )
    : String = s"${ squash(before).takeRight(reach) }[[$word]]${ squash(after)
      .take(reach) }".trim

  private def squash(text: String): String = text.replaceAll("\\s+", " ")

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
