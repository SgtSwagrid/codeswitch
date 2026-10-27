package com.alecdorrington.codeswitch

import com.alecdorrington.megalingual.Language
import com.alecdorrington.omnibrowser.StorageKey

/**
  * How the reader answers whether they knew a word above their level, each with
  * its `mark`, the familiarity that the answer moves a word towards, and its
  * `label`, as its button in the tooltip reads.
  */
enum Answer(val mark: Double, val label: String):

  case Knew extends Answer(1.0, "Knew it")

  case Unsure extends Answer(0.6, "Unsure")

  case Didnt extends Answer(0.0, "Didn't know")

/**
  * That the reader gave `answer` for `word`, a dictionary form, whose hint was
  * of `kind`, as its level, or its familiarity, had it.
  */
final case class Rating
  (
    word: String,
    kind: Hint.Kind,
    answer: Answer,
  )

/**
  * How well the reader knows each word above their level that they have rated,
  * from `0`, not at all, to `1`, well, by the word's dictionary form, which
  * decides how it is shown, in place of its level.
  */
object Familiarity:

  /** How far each answer moves a word's familiarity towards its mark. */
  val step = 0.15

  /** The familiarity from which a word is known, and only faintly marked. */
  val known = 0.8

  /** The familiarity up to which a word is shown as its meaning. */
  val unknown = 0.35

  /**
    * Where the familiarity of `word`, a dictionary form of `language`, is kept,
    * if the reader has rated it. Each word has a key of its own, so that a
    * rating in one tab is not lost to one in another, nor rewrites the rest.
    * The word keeps its case, so that `Weg`, a path, is apart from `weg`, away,
    * and the language, so that a word is apart from one spelt alike in another.
    */
  def key(language: Language, word: String): StorageKey[Option[Double]] =
    StorageKey.local[Option[Double]](
      s"familiarity:${ language.code }:${ word.trim }",
      None,
    )

  /**
    * The familiarity that a word not yet rated is taken to have, from `kind`,
    * as its level for the reader has it.
    */
  def initial(kind: Hint.Kind): Double = kind match
    case Hint.Kind.Explained => 0.5
    case Hint.Kind.Glossed   => 0.2
    case Hint.Kind.Known     => known

  /** `familiarity`, moved by [[step]] of the way towards `answer`'s mark. */
  def after(familiarity: Double, answer: Answer): Double = familiarity +
    step * (answer.mark - familiarity)

  /**
    * The familiarity of a word after `rating`, from `familiarity`, if it has
    * been rated, or else from where its kind puts it.
    */
  def rate
    (
      familiarity: Option[Double],
      rating: Rating,
    )
    : Double = after(
    familiarity.getOrElse(initial(rating.kind)),
    rating.answer,
  )

  /**
    * How a word of `familiarity` is shown: as its meaning while it is little
    * known, then as it is, with its meaning on hovering, and only faintly
    * marked once it is known, so that the reader may still say otherwise.
    */
  def kind(familiarity: Double): Hint.Kind =
    if familiarity >= known then Hint.Kind.Known
    else if familiarity > unknown then Hint.Kind.Explained
    else Hint.Kind.Glossed

  /**
    * `hint`, shown as its word's familiarity in `known` says, if the reader has
    * rated it, or else as its level says.
    */
  def apply(known: Map[String, Double], hint: Hint): Hint = known
    .get(hint.word.trim)
    .fold(hint)(familiarity => hint.copy(kind = kind(familiarity)))
