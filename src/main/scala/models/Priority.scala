package com.alecdorrington.codeswitch.models

import cats.syntax.all.*
import com.alecdorrington.megalingual.Language
import io.circe.Codec

/**
  * How a word stands with the reader: how well they know it, `familiarity` (see
  * [[services.FamiliarityService]]), and how often they have met it,
  * `frequency` (see [[services.FrequencyService]]), which is `relative` times
  * as often as the average word of its language (see
  * [[FrequencyTotal.relative]]).
  */
final case class Standing
  (
    familiarity: Double,
    frequency: Double,
    relative: Double,
  )
  derives Codec:

  /** How much the reader should focus on learning the word. */
  def priority: Double = Priority.of(relative, familiarity)

/**
  * Words of `language`, each with its CEFR level, if known, of which a page's
  * content script asks the background how each stands with the reader (see
  * [[services.PriorityService.of]]).
  */
final case class Standings
  (
    language: Language,
    words: List[(String, Option[Level])],
  )
  derives Codec

/**
  * Words of `native`, the reader's own language, of which a page's content
  * script asks the background the priority of the word of `into` that each was
  * last switched into (see [[services.PriorityService.ofForms]]).
  */
final case class Weighing
  (
    native: Language,
    into: Language,
    forms: List[String],
  )
  derives Codec

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
    * highlighted, those of the words of highest priority (see [[Highlights]]).
    */
  val share = 0.05

  /**
    * `hint`, shown as its word stands with the reader, as `standings` gives it:
    * explained, if it is among the words that `highlights` highlights, or else
    * as its familiarity says (see [[Familiarity.kind]]).
    */
  def mark
    (
      hint: Hint,
      standings: Map[String, Standing],
      highlights: Highlights,
    )
    : Hint =
    if highlights(hint.word) then hint.copy(kind = Hint.Kind.Explained)
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
    * The draw of a word drawn `drawn`, from `floor` up to `1`, as the words of
    * its text not drawn below `floor` already are, whose word in the language
    * being learnt has `priority`, if it is known: it is switched while the
    * intensity is above it. A word as many times likelier to be switched as its
    * priority makes it (see [[likelier]]) has its draw from `0` up to one over
    * that, so one not drawn below `floor` has it from `floor` up to that, where
    * `drawn` puts it.
    */
  def draw
    (
      drawn: Double,
      floor: Double,
      priority: Option[Double],
    )
    : Double = floor + (1 / likelier(priority) - floor) * (drawn - floor) /
    (1 - floor)

  /**
    * The chance that a word whose word in the language being learnt has
    * `priority`, if it is known, is switched at `intensity`, as first drawn
    * (see [[draw]]).
    */
  def chance
    (
      intensity: Double,
      priority: Option[Double],
    )
    : Double = (intensity * likelier(priority)).min(1)

  /**
    * How many of the page's words a word switched at `intensity` stands for
    * (see [[Word.times]]), whose word in the language being learnt has
    * `priority`, if it is known, switched into one of `languages`, each as
    * likely: one over its chance of being switched into its language.
    */
  def times
    (
      intensity: Double,
      priority: Option[Double],
      languages: Int,
    )
    : Double = languages / chance(intensity, priority)

/**
  * The words highlighted so far among the hints of passages asked about at
  * once, `chosen`, whose hints come to `hints`, in the passages whose hints
  * have arrived, of `words` words.
  */
final case class Highlights
  (
    words: Int,
    hints: Int,
    chosen: Set[String],
  ):

  /** Whether the hints of `word` are highlighted. */
  def apply(word: String): Boolean = chosen(word.trim)

  /**
    * These, and the words of `found`, the hints of passages of `size` words
    * that have since arrived, to highlight: those already highlighted, so that
    * a word is not highlighted in one place and not another, then those of
    * highest priority, as `standings` gives it, until the hints highlighted
    * come to [[Priority.share]] of the words of every passage so far, rounded,
    * so that passages too short for a word of their own share in one. A word of
    * no priority, as one known well, never is.
    */
  def add
    (
      found: List[Hint],
      standings: Map[String, Standing],
      size: Int,
    )
    : Highlights =
    val words    = found.map(_.word.trim)
    val priority = (word: String) => standings.get(word).fold(0.0)(_.priority)
    val allowed  = (Priority.share * (this.words + size)).round
    words
      .distinct
      .filterNot(chosen)
      .filter(priority(_) > 0)
      .sortBy(-priority(_))
      .foldLeft(copy(
        words = this.words + size,
        hints = hints + words.count(chosen),
      )):
        case (highlights, word) if highlights.hints < allowed =>
          highlights.copy(
            hints = highlights.hints + words.count(_ == word),
            chosen = highlights.chosen + word,
          )
        case (highlights, _) => highlights

object Highlights:

  /** None yet, of passages none of whose hints have arrived. */
  val none: Highlights = Highlights(0, 0, Set.empty)
