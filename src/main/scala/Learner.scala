package com.alecdorrington.codeswitch

import cats.Eq
import cats.data.NonEmptyList
import com.alecdorrington.megalingual.Language
import com.alecdorrington.megalingual.laminar.Browser
import io.circe.Codec

/**
  * A language that the reader is learning, `language`, which they read at
  * `level`.
  */
final case class Learnt(language: Language, level: Level) derives Codec:

  /**
    * Whether passages in [[language]] are read for the words above the reader's
    * level: never at C2, above which there is none, nor in a language written
    * without spaces between its words, whose words cannot yet be found.
    */
  def reads: Boolean = level != Level.C2 && language.spaced

  /**
    * How a word of `difficulty` is shown: explained if it is a level above the
    * reader's, glossed if it is further above, and otherwise not at all.
    */
  def kind(difficulty: Level): Option[Hint.Kind] =
    difficulty.ordinal - level.ordinal match
      case 1                  => Some(Hint.Kind.Explained)
      case above if above > 1 => Some(Hint.Kind.Glossed)
      case _                  => None

/**
  * The reader, who reads the `fluent` languages, their own, [[native]], first,
  * and is `learning` others. Pages in a fluent language have a share of their
  * words shown in the languages being learnt, and pages in a language being
  * learnt have their words above the reader's level marked, explained in their
  * own.
  */
final case class Learner
  (
    fluent: NonEmptyList[Language],
    learning: List[Learnt],
  ):

  /** The reader's own language, in which words are explained. */
  def native: Language = fluent.head

  /** Whether anything of a page is to be marked. */
  def active: Boolean = learning.nonEmpty

  /** The language being learnt that `language` is, in any variety, if any. */
  def learnt(language: Language): Option[Learnt] =
    learning.find(_.language.alike(language))

  /**
    * Whether a passage in `language`, if it is known, has a share of its words
    * shown in the languages being learnt: if it is in a fluent language, or in
    * one not known, but not if it is in one being learnt.
    */
  def switches(language: Option[Language]): Boolean = active && language.forall(
    language => learnt(language).isEmpty && fluent.exists(_.alike(language)),
  )

  /**
    * The language being learnt that a word is shown in, by its `draw`, from `0`
    * to `1`, which makes each as likely. There must be one.
    */
  def into(draw: Double): Language =
    learning((draw * learning.size).toInt.min(learning.size - 1)).language

object Learner:

  given Eq[Learner] = Eq.fromUniversalEquals

  /**
    * The reader, as they have chosen: their own language, `native`, or until
    * they choose it, the browser's (see [[own]]), the `fluent` others, and the
    * languages they are `learning`, each once, as one of the [[learnable]]
    * varieties.
    */
  def chosen
    (
      native: Option[Language],
      fluent: List[Language],
      learning: List[Learnt],
    )
    : Learner = chosen(
    native,
    fluent,
    learning,
    Browser.preferences,
  )

  /**
    * The reader, as they have chosen, whose own language, until they choose it,
    * is the first of `preferred` that is not being learnt (see [[own]]).
    */
  def chosen
    (
      native: Option[Language],
      fluent: List[Language],
      learning: List[Learnt],
      preferred: List[Language],
    )
    : Learner =
    val learnt = learning
      .map(learnt => learnt.copy(language = learnable.resolve(learnt.language)))
      .distinctBy(_.language.lineage.last)
    val own = native.fold(
      Learner.own(preferred, learnt.map(_.language)),
    )(learnable.resolve)
    Learner(
      NonEmptyList(
        own,
        fluent
          .map(learnable.resolve)
          .filterNot(_.alike(own))
          .distinctBy(_.lineage.last),
      ),
      learnt,
    )

  /**
    * The reader's own language, until they choose one: the first of the
    * browser's that is not one being `learnt`, or else English.
    */
  def own(learnt: List[Language]): Language = own(Browser.preferences, learnt)

  /**
    * The reader's own language, until they choose one: the first of `preferred`
    * that is not one being `learnt`, in any variety, or else English, as one of
    * the [[learnable]] varieties.
    */
  def own
    (
      preferred: List[Language],
      learnt: List[Language],
    )
    : Language = learnable.resolve(
    preferred
      .find(language => !learnt.exists(language.alike))
      .getOrElse(Language.English),
  )
