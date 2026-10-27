package com.alecdorrington.codeswitch

import cats.Eq
import com.alecdorrington.megalingual.Language
import com.alecdorrington.megalingual.laminar.Browser

/**
  * The reader, as one learning `language`, whose own is `native`, and who knows
  * it at `level`. If `translating`, pages in other languages have a share of
  * their words shown in `language`, and if `reading`, pages in `language` have
  * their words above the reader's level marked.
  */
final case class Learner
  (
    language: Language,
    native: Language,
    level: Level,
    translating: Boolean,
    reading: Boolean,
  ):

  /**
    * Whether pages in [[language]] are read for the words above the reader's
    * level: never at C2, above which there is none, nor in a language written
    * without spaces between its words, whose words cannot yet be found.
    */
  def reads: Boolean = reading && level != Level.C2 && language.spaced

  /** Whether anything of a page is to be marked. */
  def active: Boolean = translating || reads

  /**
    * How a word of `difficulty` is shown: explained if it is a level above the
    * reader's, glossed if it is further above, and otherwise not at all.
    */
  def kind(difficulty: Level): Option[Hint.Kind] =
    difficulty.ordinal - level.ordinal match
      case 1                  => Some(Hint.Kind.Explained)
      case above if above > 1 => Some(Hint.Kind.Glossed)
      case _                  => None

object Learner:

  given Eq[Learner] = Eq.fromUniversalEquals

  /**
    * The reader's own language, until they choose one: the first of the
    * browser's that is not the one being `learnt`, or else English.
    */
  def own(learnt: Option[Language]): Language = own(Browser.preferences, learnt)

  /**
    * The reader's own language, until they choose one: the first of `preferred`
    * that is not the one being `learnt`, in any variety, or else English, as
    * one of the [[learnable]] varieties.
    */
  def own
    (
      preferred: List[Language],
      learnt: Option[Language],
    )
    : Language = learnable.resolve(
    preferred
      .find(language => !learnt.exists(language.alike))
      .getOrElse(Language.English),
  )
