package com.alecdorrington.codeswitch

import cats.effect.{IO, SyncIO}
import cats.effect.std.Semaphore
import cats.syntax.all.*
import com.alecdorrington.megalingual.Language
import com.alecdorrington.omnibrowser.{Browser, StorageKey, StorageKeys}
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

/**
  * The one source of how often the reader has met each word of a language being
  * learnt on the pages they read, kept in `storage`, `0` for a word never met,
  * of the total of each language (see [[FrequencyTotal]]), and of the word that
  * each word of their own language was last switched into.
  *
  * Any part may read, but only the background writes, as it answers the `hints`
  * and `translate` messages, one count at a time, so that two tabs counting a
  * word at once lose neither, and each total stays the sum of its words'
  * counts.
  */
final class FrequencyService(storage: Browser.Storage):

  /**
    * How often each of `words`, of `language`, has been met, by the word, `0`
    * for one never met.
    */
  def of(language: Language, words: List[String]): IO[Map[String, Double]] =
    words
      .map(_.trim)
      .distinct
      .parTraverse(word =>
        storage.get(Frequency.key(language, word)).tupleLeft(word),
      )
      .map(_.toMap)

  /** How often the words of `language` have been met in all. */
  def total(language: Language): IO[FrequencyTotal] =
    storage.get(Frequency.totalKey(language))

  /**
    * Counts that each of `words`, of `language`, was met once more. Only the
    * background calls it.
    */
  def met(language: Language, words: List[String]): IO[Unit] =
    count(language, words.map(_ -> 1.0))

  /**
    * Counts that each of `words`, of `language`, was met as many more times as
    * it is given, those of a word given several times added together, and adds
    * them to the language's total. Only the background calls it.
    */
  def count
    (
      language: Language,
      words: List[(String, Double)],
    )
    : IO[Unit] = FrequencyService
    .lock
    .permit
    .surround(
      Frequency
        .tally(words)
        .traverse((word, times) => add(Frequency.key(language, word), times))
        .flatMap(added =>
          storage
            .update(Frequency.totalKey(language))(total =>
              added.foldLeft(total)((total, met) => total.add(met._1, met._2)),
            )
            .whenA(added.nonEmpty),
        ),
    )

  /**
    * Adds `times` to the count kept as `key`, and gives the count before, with
    * `times`.
    */
  private def add
    (key: StorageKey[Double], times: Double)
    : IO[(Double, Double)] = storage
    .get(key)
    .flatTap(before => storage.set(key, before + times))
    .tupleRight(times)

  /**
    * Keeps the word that each of `switches`, of `native`, was switched into,
    * and counts that the word was met as often as the switch stands for. Only
    * the background calls it.
    */
  def switched(native: Language, switches: List[Switch]): IO[Unit] = switches
    .filter(_.as.word.trim.nonEmpty)
    .groupBy(_.into)
    .toList
    .traverse_((into, switches) =>
      switches.traverse_(switch =>
        storage.set(
          Frequency.switchKey(native, into, switch.form),
          Some(switch.as.copy(word = switch.as.word.trim)),
        ),
      ) *> count(
        into,
        switches.map(switch => switch.as.word -> switch.times),
      ),
    )

  /**
    * The word of `into` that each of `forms`, words of `native`, was last
    * switched into, by the form, for each that has been.
    */
  def switchedOf
    (
      native: Language,
      into: Language,
      forms: List[String],
    )
    : IO[Map[String, Switched]] = forms
    .distinct
    .parTraverse(form =>
      storage.get(Frequency.switchKey(native, into, form)).map(_.map(form -> _)),
    )
    .map(_.flatten.toMap)

  /**
    * How often each word met has been, by the name of its key, as its rating's
    * is named (see [[Familiarity.key]]).
    */
  def all: IO[Map[String, Double]] = storage.all(Frequency.counts)

  /** How often the words of each language have been met, by its whole code. */
  def totals: IO[Map[String, FrequencyTotal]] = storage.all(Frequency.totals)

object FrequencyService:

  /**
    * What lets the background count one change at a time, lest two that overlap
    * lose one, as each reads the count before writing it.
    */
  private lazy val lock = Semaphore.in[SyncIO, IO](1).unsafeRunSync()

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
