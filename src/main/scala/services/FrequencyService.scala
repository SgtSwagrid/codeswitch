package com.alecdorrington.codeswitch.services

import cats.effect.{IO, SyncIO}
import cats.effect.std.Semaphore
import cats.syntax.all.*
import com.alecdorrington.codeswitch.lexicon.Store
import com.alecdorrington.codeswitch.models.{
  Frequency, FrequencyTotal, Switch, Switched,
}
import com.alecdorrington.megalingual.Language

/**
  * The one source of how often the reader has met each word of a language being
  * learnt on the pages they read, kept in the lexicon's `store`, `0` for a word
  * never met, of the total of each language (see [[models.FrequencyTotal]]),
  * and of the word that each word of their own language was last switched into.
  *
  * The background and the extension's pages may read, but only the background
  * writes, as it answers the `hints`, `count` and `translate` messages, one
  * count at a time, so that two tabs counting a word at once lose neither, and
  * each total stays the sum of its words' counts, each count kept with its
  * total in one transaction. The pages' content scripts ask the background (see
  * [[PriorityService]]).
  */
final class FrequencyService(store: Store):

  /**
    * How often each of `words`, of `language`, has been met, by the word, `0`
    * for one never met.
    */
  def of(language: Language, words: List[String]): IO[Map[String, Double]] =
    val named = words.map(_.trim).distinct
    store
      .counts(named.map(Frequency.nameOf(language, _)))
      .map(counts =>
        named
          .map(word =>
            word -> counts.getOrElse(Frequency.nameOf(language, word), 0.0),
          )
          .toMap,
      )

  /** How often the words of `language` have been met in all. */
  def total(language: Language): IO[FrequencyTotal] =
    val name = Frequency.totalOf(language)
    store.totals(List(name)).map(_.getOrElse(name, FrequencyTotal.none))

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
    : IO[Unit] = tally(language, words, Map.empty)

  /**
    * Counts `words`, of `language`, as [[count]] does, and keeps `switches`, by
    * their names, all at once, one change at a time.
    */
  private def tally
    (
      language: Language,
      words: List[(String, Double)],
      switches: Map[String, Switched],
    )
    : IO[Unit] =
    val tallied = Frequency
      .tally(words)
      .map((word, times) => Frequency.nameOf(language, word) -> times)
    val total = Frequency.totalOf(language)
    FrequencyService
      .lock
      .permit
      .surround(
        (store.counts(tallied.map(_._1)), store.totals(List(total))).flatMapN(
          (counts, totals) =>
            val met = tallied.map((name, times) =>
              (name, counts.getOrElse(name, 0.0), times),
            )
            store.tally(
              met.map((name, before, times) => name -> (before + times)).toMap,
              Option
                .when(met.nonEmpty)(
                  total -> met.foldLeft(
                    totals.getOrElse(total, FrequencyTotal.none),
                  )((total, met) => total.add(met._2, met._3)),
                )
                .toMap,
              switches,
            ),
        ),
      )
      .whenA(tallied.nonEmpty || switches.nonEmpty)

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
      tally(
        into,
        switches.map(switch => switch.as.word -> switch.times),
        switches
          .map(switch =>
            Frequency.switchOf(native, into, switch.form) ->
              switch.as.copy(word = switch.as.word.trim),
          )
          .toMap,
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
    : IO[Map[String, Switched]] =
    val named = forms
      .distinct
      .map(form => form -> Frequency.switchOf(native, into, form))
    store
      .switches(named.map(_._2))
      .map(switches =>
        named.flatMap((form, name) => switches.get(name).map(form -> _)).toMap,
      )

  /**
    * How often each word met has been, by its name, as its rating's is named
    * (see [[models.Familiarity.nameOf]]).
    */
  def all: IO[Map[String, Double]] = store.allCounts

  /** How often the words of each language have been met, by its whole code. */
  def totals: IO[Map[String, FrequencyTotal]] = store.allTotals

object FrequencyService:

  /**
    * What lets the background count one change at a time, lest two that overlap
    * lose one, as each reads the counts and the total before writing them.
    */
  private lazy val lock = Semaphore.in[SyncIO, IO](1).unsafeRunSync()
