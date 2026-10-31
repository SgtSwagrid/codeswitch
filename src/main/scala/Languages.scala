package com.alecdorrington.codeswitch

import com.alecdorrington.megalingual.{Language, Localised}
import com.alecdorrington.megalingual.Language.{
  Chinese, GermanOfSwitzerland, Japanese, Thai,
}

/**
  * The languages a reader may learn or read in: every one, a broader language
  * (German) as itself, but Chinese, which is written in two scripts, and German
  * (Switzerland), which is both Swiss Standard German and Swiss German, only in
  * one of their varieties, lest a page mix them. Each of those, chosen, stored
  * or preferred by a browser (`zh`, `de-CH`), reads as its first, Simplified
  * Chinese or Swiss Standard German. The pickers offer the `listed` languages,
  * those two among them, each variety beneath its own.
  */
val learnable: Localised[Language] = Localised.only(
  Language.English,
  Language.values.toList.filterNot(Set(Chinese, GermanOfSwitzerland))*,
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

extension (language: Language)

  /**
    * The whole language that this is a variety of, as it is learnt, read and
    * looked up: the broadest of its lineage under its own primary tag, so
    * English of British English and German of Swiss Standard German, but Swiss
    * German itself (`gsw`), a language of its own, though Megalingual has it
    * beneath German (Switzerland), as its readers' browsers send `de-CH`.
    */
  def whole: Language = language
    .lineage
    .findLast(primary(_) == primary(language))
    .getOrElse(language)

  /**
    * Whether the language is `other`, whatever the variety of either (British
    * and American English are alike), as a page in either is read alike. Swiss
    * German is not German (see [[whole]]).
    */
  def alike(other: Language): Boolean = language.whole == other.whole

/** The language subtag of `language`'s code, `de` of `de-CH-1996`. */
private def primary(language: Language): String = language
  .code
  .takeWhile(_ != '-')
