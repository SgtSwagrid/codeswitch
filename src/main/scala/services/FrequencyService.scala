package com.alecdorrington.codeswitch.services

import cats.effect.{IO, SyncIO}
import cats.effect.std.Semaphore
import cats.syntax.all.*
import com.alecdorrington.codeswitch.models.{
  Frequency, FrequencyTotal, Switch, Switched,
}
import com.alecdorrington.megalingual.Language
import com.alecdorrington.omnibrowser.{Browser, StorageKey}

/**
  * The one source of how often the reader has met each word of a language being
  * learnt on the pages they read, kept in `storage`, `0` for a word never met,
  * of the total of each language (see [[models.FrequencyTotal]]), and of the
  * word that each word of their own language was last switched into.
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
    * is named (see [[models.Familiarity.key]]).
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
