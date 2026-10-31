package com.alecdorrington.codeswitch

import com.alecdorrington.megalingual.{Language, Localised}
import com.alecdorrington.megalingual.Language.{Chinese, Japanese, Thai}

/**
  * The languages a reader may learn or read in: every one, a broader language
  * (German) as itself, but Chinese, which is written in two scripts, only in
  * one of its varieties, lest a page mix them. Chinese itself, chosen, stored
  * or preferred by a browser (`zh`), reads as its first, Simplified Chinese.
  * The pickers offer the `listed` languages, Chinese among them, each variety
  * beneath its own.
  */
val learnable: Localised[Language] = Localised.only(
  Language.English,
  Language.values.toList.filterNot(_ == Chinese)*,
)

/**
  * Whether the language's words are written apart, so that they can be found:
  * not in Chinese, Japanese or Thai.
  */
extension (language: Language)

  def spaced: Boolean = !language.within(Chinese) && language != Japanese &&
    language != Thai

/**
  * The language of a passage: the one that the browser found reliably, `tag`,
  * if it is one known, or else the one that the page says it is, `declared`.
  */
def foundLanguage
  (
    tag: Option[String],
    declared: => Option[Language],
  )
  : Option[Language] = tag.flatMap(Language.fromTag).orElse(declared)

/**
  * How many words of the passages of a page were found in each language in
  * `pass`, the language of most of which is the page's own.
  */
final case class Tally(pass: Int, words: Map[Language, Int]):

  /**
    * It, with `count` more words found in `language` in `pass`, counted afresh
    * if that is a pass after its own.
    */
  def add(pass: Int, language: Language, count: Int): Tally =
    if pass == this.pass then
      copy(words =
        words.updated(
          language,
          words.getOrElse(language, 0) + count,
        ),
      )
    else Tally(pass, Map(language -> count))

  /** The language of most of the words found in `pass`, if any were. */
  def prevailing(pass: Int): Option[Language] = Option
    .when(pass == this.pass)(words)
    .flatMap(_.maxByOption(_._2))
    .map(_._1)

object Tally:

  /** No words found, in no pass. */
  val empty: Tally = Tally(-1, Map.empty)

/**
  * Whether the language is `other`, whatever the variety of either (British and
  * American English are alike), as a page in either is read alike.
  */
extension (language: Language)

  def alike(other: Language): Boolean = language.lineage.last ==
    other.lineage.last
