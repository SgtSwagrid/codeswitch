package com.alecdorrington.codeswitch

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
  * That the reader gave `answer` for `word`, a dictionary form, which was shown
  * as `kind`.
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

  /** The familiarity from which a word is no longer marked. */
  val known = 0.8

  /** The familiarity up to which a word is shown as its meaning. */
  val unknown = 0.35

  /**
    * The familiarity that a word not yet rated is taken to have, if shown as
    * `kind`, its level for the reader.
    */
  def initial(kind: Hint.Kind): Double = kind match
    case Hint.Kind.Explained => 0.5
    case Hint.Kind.Glossed   => 0.2

  /** `familiarity`, moved by [[step]] of the way towards `answer`'s mark. */
  def after(familiarity: Double, answer: Answer): Double = familiarity +
    step * (answer.mark - familiarity)

  /**
    * `known`, the familiarities of the words that the reader has rated, after
    * `answer` for `word`, shown as `kind`.
    */
  def rate
    (
      known: Map[String, Double],
      word: String,
      kind: Hint.Kind,
      answer: Answer,
    )
    : Map[String, Double] = known.updated(
    keyOf(word),
    after(
      known.getOrElse(keyOf(word), initial(kind)),
      answer,
    ),
  )

  /**
    * How a word of `familiarity` is shown: as its meaning while it is little
    * known, then as it is, with its meaning on hovering, and not at all once it
    * is known.
    */
  def kind(familiarity: Double): Option[Hint.Kind] =
    if familiarity >= known then None
    else if familiarity > unknown then Some(Hint.Kind.Explained)
    else Some(Hint.Kind.Glossed)

  /**
    * `hint`, shown as `known` says of its word, if the reader has rated it, or
    * else as its level says.
    */
  def apply(known: Map[String, Double], hint: Hint): Option[Hint] = known
    .get(keyOf(hint.word))
    .fold(Some(hint.kind))(kind)
    .map(kind => hint.copy(kind = kind))

  /** The key of `word`, a dictionary form, in the words rated. */
  def keyOf(word: String): String = word.trim.toLowerCase
