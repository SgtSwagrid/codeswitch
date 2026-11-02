package com.alecdorrington.codeswitch.models

import cats.syntax.all.*
import com.alecdorrington.megalingual.Language
import com.alecdorrington.omnibrowser.{StorageKey, StorageKeys}
import io.circe.Codec

/**
  * That `form`, a word of a page in the reader's own language, was switched
  * into `as`, a word of `into`, standing for `times` of the page's words (see
  * [[Word.times]]).
  */
final case class Switch
  (
    form: String,
    as: Switched,
    into: Language,
    times: Double,
  )

object Switch:

  /**
    * The switch that `translation` makes of `word`, standing for as many of the
    * page's words as `word` claims, within bounds, unless it declines it.
    */
  def of(word: Word, translation: Translation): Option[Switch] =
    Option.when(translation.pieces.nonEmpty)(Switch(
      word.text,
      Switched(translation.word, translation.level),
      word.into,
      Frequency.bounded(word.times),
    ))

/**
  * The word that a word of the reader's own language was last switched into:
  * its dictionary form, `word`, and its CEFR `level`, its difficulty, if known.
  */
final case class Switched(word: String, level: Option[Level]) derives Codec

/**
  * How often the words of a language have been met in all, `frequency`, the sum
  * of their counts, and how many have been, `lexemes`, by which each word's
  * count is weighed against the rest's.
  */
final case class FrequencyTotal(frequency: Double, lexemes: Int) derives Codec:

  /** How often the average word met has been met, `0` if none has been. */
  def average: Double = if lexemes > 0 then frequency / lexemes else 0

  /**
    * How often a word met `frequency` times has been, against the average
    * word's: `1` for one met as often, `0` if none has been met.
    */
  def relative(frequency: Double): Double =
    if average > 0 then frequency / average else 0

  /** This total, once a word met `before` times has been met `times` more. */
  def add(before: Double, times: Double): FrequencyTotal = FrequencyTotal(
    frequency + times,
    if before > 0 then lexemes else lexemes + 1,
  )

object FrequencyTotal:

  /** The total of a language none of whose words has been met. */
  val none: FrequencyTotal = FrequencyTotal(0, 0)

/** How often the reader has met each word of a language being learnt. */
object Frequency:

  /**
    * How often each word of a language being learnt has been met, by the name
    * of its rating (see [[Familiarity.key]]), `0` until it is first met.
    */
  val counts = StorageKeys.local[Double]("frequency:", 0)

  /**
    * How often the words of each language being learnt have been met in all, by
    * the language's whole code.
    */
  val totals =
    StorageKeys.local[FrequencyTotal]("frequencyTotal:", FrequencyTotal.none)

  /**
    * The word of a language being learnt that each word of the reader's own was
    * last switched into, by both languages and the word.
    */
  val switches = StorageKeys.local[Option[Switched]]("switched:", None)

  /**
    * Where how often `word`, a dictionary form of `language`, has been met is
    * kept, by the same name as its rating (see [[Familiarity.key]]).
    */
  def key(language: Language, word: String): StorageKey[Double] =
    counts(Familiarity.nameOf(language, word))

  /**
    * Where how often the words of `language` have been met in all is kept, by
    * the whole language's code, as each word is.
    */
  def totalKey(language: Language): StorageKey[FrequencyTotal] =
    totals(language.whole.code)

  /**
    * Where the word of `into` that `form`, a word of `native`, was last
    * switched into is kept: by the whole languages' codes, then the form in
    * lower case, as a word that begins a sentence is the word that does not.
    */
  def switchKey
    (
      native: Language,
      into: Language,
      form: String,
    )
    : StorageKey[Option[Switched]] = switches(
    s"${ native.whole.code }:${ into.whole.code }:${ form.trim.toLowerCase }",
  )

  /**
    * Each of `words` that has a word and is met at least once more, trimmed,
    * with how many more times it is met in all.
    */
  def tally(words: List[(String, Double)]): List[(String, Double)] = words
    .map((word, times) => word.trim -> times)
    .filter((word, times) => word.nonEmpty && times > 0)
    .groupMapReduce(_._1)(_._2)(_ + _)
    .toList

  /**
    * The most of the page's words that one word switched may stand for, as a
    * page's content script may claim any.
    */
  val most = 1000.0

  /**
    * The number of the page's words that a word stands for, `times`, kept from
    * `0` up to [[most]].
    */
  def bounded(times: Double): Double =
    if times.isNaN then 0 else times.max(0).min(most)
