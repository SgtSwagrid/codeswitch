package com.alecdorrington.codeswitch

import cats.effect.IO
import cats.syntax.all.*
import com.alecdorrington.megalingual.Language

/**
  * How a word stands with the reader: how well they know it, `familiarity` (see
  * [[FamiliarityService]]), and how often they have met it, `frequency` (see
  * [[FrequencyService]]), which is `relative` times as often as the average
  * word of its language (see [[FrequencyTotal.relative]]).
  */
final case class Standing
  (
    familiarity: Double,
    frequency: Double,
    relative: Double,
  ):

  /** How much the reader should focus on learning the word. */
  def priority: Double = Priority.of(relative, familiarity)

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

/** How much the reader should focus on learning each word. */
object Priority:

  /**
    * How much the reader should focus on learning a word met `relative` times
    * as often as the average word of its language, that they know as well as
    * `familiarity` says: hardly at all if they are likely to know it, however
    * often they meet it, and otherwise the more, the more often they meet it,
    * up to about as often as the average word, beyond which it matters little
    * more, so that the commonest words, as articles, do not crowd out the rest.
    */
  def of(relative: Double, familiarity: Double): Double =
    math.pow((1 - familiarity).max(0), 2) * -math.expm1(-relative.max(0))

  /**
    * The share of the words of the passages asked about at once whose hints are
    * highlighted, those of the words of highest priority.
    */
  val share = 0.05

  /**
    * The words of `hints` to highlight, those of highest priority, as
    * `standings` gives it, until their hints come to [[share]] of `size`, the
    * number of words of their passages. A word is highlighted wherever it is,
    * so that it is not highlighted in one place and not another; a word of no
    * priority, as one known well, never is.
    */
  def highlighted
    (
      hints: List[Hint],
      standings: Map[String, Standing],
      size: Int,
    )
    : Set[String] =
    val words    = hints.map(_.word.trim)
    val priority = (word: String) => standings.get(word).fold(0.0)(_.priority)
    words
      .distinct
      .filter(priority(_) > 0)
      .sortBy(-priority(_))
      .foldLeft((Set.empty[String], 0)):
        case ((chosen, count), word) if count < (share * size).round =>
          (chosen + word, count + words.count(_ == word))
        case (done, _) => done
      ._1

  /**
    * `hint`, shown as its word stands with the reader, as `standings` gives it:
    * explained, if it is among the words `highlighted`, or else as its
    * familiarity says (see [[Familiarity.kind]]).
    */
  def mark
    (
      hint: Hint,
      standings: Map[String, Standing],
      highlighted: Set[String],
    )
    : Hint =
    if highlighted(hint.word.trim) then hint.copy(kind = Hint.Kind.Explained)
    else
      standings
        .get(hint.word.trim)
        .fold(hint)(standing =>
          hint.copy(kind = Familiarity.kind(standing.familiarity)),
        )

  /** The most times likelier than a typical word that a word is switched. */
  val most = 4.0

  /** The fewest times as likely as a typical word that a word is switched. */
  val least = 0.1

  /**
    * The priority of a typical word: one met as often as the average, of no
    * level known, not yet rated (see [[Familiarity.default]]).
    */
  private lazy val typical = of(1, Familiarity.default(None, None))

  /**
    * How many times likelier than a typical word a word is to be switched,
    * whose word in the language being learnt has `priority`, if it is known: as
    * many times as its priority is the typical word's, from [[least]] up to
    * [[most]], or as likely, if not.
    */
  private def likelier(priority: Option[Double]): Double =
    priority.fold(1.0)(priority => (priority / typical).max(least).min(most))

  /**
    * The draw of a word drawn `drawn`, from `0` to `1`, whose word in the
    * language being learnt has `priority`, if it is known: it is switched while
    * the intensity is above it, so that it is as many times likelier to be as
    * its priority is the typical word's, within bounds (see [[likelier]]).
    */
  def draw(drawn: Double, priority: Option[Double]): Double = drawn /
    likelier(priority)

  /**
    * The chance that a word whose word in the language being learnt has
    * `priority`, if it is known, is switched at `intensity` (see [[draw]]).
    */
  def chance
    (
      intensity: Double,
      priority: Option[Double],
    )
    : Double = (intensity * likelier(priority)).min(1)

  /**
    * The draw of a word drawn `drawn`, whose word in the language being learnt
    * has `priority`, if it is known, and how many of the page's words it stands
    * for (see [[Word.times]]), if it is switched at `intensity`: one over its
    * chance, if its text's words are drawn for the `first` time, or else none.
    */
  def switched
    (
      drawn: Double,
      priority: Option[Double],
      intensity: Double,
      first: Boolean,
    )
    : Option[(Double, Double)] =
    val shown = draw(drawn, priority)
    Option.when(shown < intensity)(
      shown -> (if first then 1 / chance(intensity, priority) else 0),
    )
