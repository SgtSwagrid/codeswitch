package com.alecdorrington.codeswitch.services

import cats.effect.IO
import cats.syntax.all.*
import com.alecdorrington.codeswitch.models.{
  whole, Familiarity, FrequencyTotal, Level, Lexeme, Standing,
}
import com.alecdorrington.megalingual.Language

/**
  * How much the reader should focus on learning each word, from how well they
  * know it, as `familiarity` gives it, and how often they have met it, as
  * `frequency` does, keeping nothing of its own.
  */
final class PriorityService
  (
    familiarity: FamiliarityService,
    frequency: FrequencyService,
  ):

  /**
    * How each of `words`, of `language`, each with its CEFR level, if known,
    * stands with the reader, by the word.
    */
  def of
    (
      language: Language,
      words: List[(String, Option[Level])],
    )
    : IO[Map[String, Standing]] = (
    familiarity.of(language, words),
    frequency.of(language, words.map(_._1)),
    frequency.total(language),
  ).mapN((familiarities, frequencies, total) =>
    familiarities.map((word, familiarity) =>
      val met = frequencies.getOrElse(word, 0.0)
      word -> Standing(familiarity, met, total.relative(met)),
    ),
  )

  /**
    * The priority of the word of `into` that each of `forms`, words of
    * `native`, was last switched into, by the form, for each that has been.
    */
  def ofForms
    (
      native: Language,
      into: Language,
      forms: List[String],
    )
    : IO[Map[String, Double]] = frequency
    .switchedOf(native, into, forms)
    .flatMap(switched =>
      of(
        into,
        switched.values.toList.map(as => as.word -> as.level),
      ).map(standings =>
        switched.flatMap((form, as) =>
          standings.get(as.word).map(form -> _.priority),
        ),
      ),
    )

  /**
    * Every word that the reader has rated or seen, with how well they know it
    * and how often they have met it.
    */
  def all: IO[List[Lexeme]] = (familiarity.all, frequency.all, frequency.totals)
    .mapN((lexemes, frequencies, totals) =>
      lexemes.map(lexeme =>
        val met = frequencies.getOrElse(
          Familiarity.nameOf(lexeme.language, lexeme.word),
          0.0,
        )
        lexeme.copy(
          frequency = met,
          relative = totals
            .getOrElse(
              lexeme.language.whole.code,
              FrequencyTotal.none,
            )
            .relative(met),
        ),
      ),
    )
