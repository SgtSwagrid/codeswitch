package com.alecdorrington.codeswitch.models

import cats.syntax.all.*
import com.alecdorrington.megalingual.Language
import com.alecdorrington.omnibrowser.{StorageKey, StorageKeys}
import io.circe.Codec

/**
  * How well the reader answers that they knew a word, on a scale of five, from
  * not at all to well, each with its `pull`, how far it moves the word's
  * familiarity, as a share of [[Familiarity.step]], down towards `0` if it is
  * less than `0`, or up towards `1` if more, and its `label`, what it means.
  * Only knowing a word well raises it; knowing it mostly leaves it be.
  */
enum Answer(val pull: Double, val label: String) derives Codec:

  case Didnt extends Answer(-1.0, "Didn't know")

  case Barely extends Answer(-2.0 / 3, "Barely knew it")

  case Unsure extends Answer(-1.0 / 3, "Unsure")

  case Mostly extends Answer(0.0, "Mostly knew it")

  case Knew extends Answer(1.0, "Knew it")

  /** Where it is on the scale, from `1` to `5`, as its button reads. */
  def point: Int = ordinal + 1

/**
  * That the reader gave `answer` for `word`, a dictionary form of `language`,
  * whose CEFR level, its difficulty, is `level`, if known.
  */
final case class Rating
  (
    word: String,
    language: Language,
    level: Option[Level],
    answer: Answer,
  )
  derives Codec

/**
  * That the reader has seen a word shown on a page, whose CEFR level, its
  * difficulty, is `level`, if known, as a `wordClass` meaning `meaning` where
  * they last saw it, if known, in whose sense its card looks it up.
  */
final case class Sighting
  (
    level: Option[Level],
    wordClass: String,
    meaning: String,
  )
  derives Codec

/**
  * The sum of the familiarities of every word that the reader has rated or
  * seen, `sum`, as of their levels in the languages they were learning,
  * `learning`, against which a word not rated was taken to be known.
  */
final case class Total(sum: Double, learning: List[Learnt]) derives Codec

/**
  * A word of a language being learnt that the reader has rated or seen, `word`,
  * a dictionary form of `language`, how well they know it, `familiarity` (see
  * [[services.FamiliarityService]]), if it has been seen, as what it was last
  * seen (see [[Sighting]]), and how often they have met it, `frequency` (see
  * [[services.FrequencyService]]), which is `relative` times as often as the
  * average word of its language.
  */
final case class Lexeme
  (
    word: String,
    language: Language,
    familiarity: Double,
    seen: Option[Sighting] = None,
    frequency: Double = 0,
    relative: Double = 0,
  ):

  /** How the word stands with the reader. */
  def standing: Standing = Standing(familiarity, frequency, relative)

  /** How much the reader should focus on learning the word. */
  def priority: Double = standing.priority

  /**
    * The word that its card is of, in the sense in which it was last seen, if
    * it has been, showing its meaning there.
    */
  def headword: Headword =
    val meaning = seen.fold("")(_.meaning)
    Headword(
      word,
      language,
      seen.fold("")(_.wordClass),
      meaning,
      if meaning.isEmpty then word else meaning,
      seen.flatMap(_.level),
    )

/**
  * How well the reader knows a word, from `0`, not at all, to `1`, well, which
  * decides how it is shown, by its dictionary form: as they rated it, or else
  * as its difficulty for them puts it.
  */
object Familiarity:

  /**
    * How far an answer moves a word's familiarity at most: the share of the way
    * to `0`, or to `1`, that it goes.
    */
  val step = 0.15

  /**
    * The familiarity from which a word not among those of highest priority is
    * shown as it is, only faintly marked, and below which as its gloss (see
    * [[kind]]).
    */
  val familiar = 0.5

  /** The most words that one message keeps as seen. */
  val most = 200

  /**
    * `rating`, its word trimmed, unless that is empty or longer than
    * [[Lookup.longest]], as a page's content script may send anything.
    */
  def checked(rating: Rating): Option[Rating] =
    word(rating.word).map(word => rating.copy(word = word))

  /**
    * `headword`, of its word trimmed, its meaning, tip and form trimmed and cut
    * to [[Lookup.longest]], and its word class only if it is one of
    * [[Lookup.wordClasses]], unless its word is empty or longer than
    * [[Lookup.longest]], as a page's content script may send anything.
    */
  def checked(headword: Headword): Option[Headword] =
    word(headword.word).map(word =>
      headword.copy(
        word = word,
        wordClass = Lookup.wordClass(headword.wordClass),
        meaning = headword.meaning.trim.take(Lookup.longest),
        tip = headword.tip.trim.take(Lookup.longest),
        form = headword.form.trim.take(Lookup.longest),
      ),
    )

  /** `word`, trimmed, unless it is empty or longer than [[Lookup.longest]]. */
  private def word(word: String): Option[String] = Some(word.trim).filter(
    word => word.nonEmpty && word.length <= Lookup.longest,
  )

  /**
    * The familiarity that a word at the reader's level, not rated, is taken to
    * have (see [[default]]).
    */
  val known = 0.8

  /** The familiarity of each word that the reader has rated (see [[key]]). */
  val words = StorageKeys.local[Option[Double]]("familiarity:", None)

  /**
    * The difficulty of each word that the reader has seen shown on a page (see
    * [[seenKey]]), kept apart from [[words]], so that a page showing a word
    * never rewrites the reader's rating of it.
    */
  val seen = StorageKeys.local[Option[Sighting]]("seen:", None)

  /**
    * The sum of the familiarities of every word that the reader has rated or
    * seen, which [[services.FamiliarityService]] keeps with each write.
    */
  val total = StorageKey.local[Option[Total]]("familiaritySum", None)

  /**
    * Where the familiarity of `word`, a dictionary form of `language`, is kept,
    * if the reader has rated it. Each word has a key of its own, so that a
    * rating in one tab is not lost to one in another, nor rewrites the rest.
    * The word keeps its case, so that `Weg`, a path, is apart from `weg`, away,
    * and the language, so that a word is apart from one spelt alike in another.
    * The language is the whole one, not its variety (see [[whole]]), so that a
    * word rated in German (Germany) is known in Swiss Standard German too, but
    * not in Swiss German, a language of its own, and a language stored as a
    * whole one, as before varieties, is rated and read under one key.
    */
  def key(language: Language, word: String): StorageKey[Option[Double]] =
    words(nameOf(language, word))

  /**
    * Where that the reader has seen `word`, a dictionary form of `language`, is
    * kept, by the same name as its rating (see [[key]]).
    */
  def seenKey(language: Language, word: String): StorageKey[Option[Sighting]] =
    seen(nameOf(language, word))

  /**
    * The name of the keys of `word` in [[words]] and [[seen]], of its whole
    * language's code then the word, as of its count (see [[Frequency.key]]).
    */
  private[codeswitch] def nameOf(language: Language, word: String): String =
    s"${ language.whole.code }:${ word.trim }"

  /**
    * That a word kept as `kept`, if at all, has been seen again as `headword`:
    * of its level, if known, or else of the level it was known to be before, in
    * its sense, if known, or else in the sense it was seen in before.
    */
  def sighting
    (
      kept: Option[Sighting],
      headword: Headword,
    )
    : Option[Sighting] = Some(Sighting(
    headword.level.orElse(kept.flatMap(_.level)),
    Some(headword.wordClass.trim)
      .filter(_.nonEmpty)
      .orElse(kept.map(_.wordClass))
      .getOrElse(""),
    Some(headword.meaning.trim)
      .filter(_.nonEmpty)
      .orElse(kept.map(_.meaning))
      .getOrElse(""),
  ))

  /**
    * The familiarity that a word the reader has not rated is taken to have,
    * from its difficulty, its CEFR `level`, if known, against the reader's
    * level in its language, `learnt`, if they are learning it (see
    * [[byLevel]]); or else, not knowing both, as a word a level above theirs.
    */
  def default
    (
      level: Option[Level],
      learnt: Option[Learnt],
    )
    : Double = (level, learnt)
    .mapN((level, learnt) => byLevel(level.ordinal - learnt.level.ordinal))
    .getOrElse(byLevel(1))

  /**
    * The familiarity that a word not rated is taken to have, `above` levels
    * above the reader's: little if it is above theirs, the less the further
    * above, and much at or below it, the more the further below, so that an
    * easy word is hardly ever thought worth learning, however often it is met
    * (see [[Priority.of]]).
    */
  private def byLevel(above: Int): Double = above match
    case above if above > 1 => 0.2
    case 1                  => 0.5
    case 0                  => known
    case -1                 => 0.9
    case _                  => 0.95

  /**
    * How well the reader knows a word: as `saved`, if they rated it, or else by
    * default, from its `level`, if known, against theirs, `learnt` (see
    * [[default]]).
    */
  def of
    (
      saved: Option[Double],
      level: Option[Level],
      learnt: Option[Learnt],
    )
    : Double = saved.getOrElse(default(level, learnt))

  /**
    * The part of the sum of familiarities that a word has, rated `saved` and
    * seen as `seen`, if at all: how well the reader knows it, as of their level
    * in its language, `learnt`, if it is rated or seen, or else none.
    */
  def share
    (
      saved: Option[Double],
      seen: Option[Sighting],
      learnt: Option[Learnt],
    )
    : Double =
    if saved.isEmpty && seen.isEmpty then 0
    else of(saved, seen.flatMap(_.level), learnt)

  /**
    * `familiarity`, moved by `answer`: its pull's share of [[step]] of the way
    * down to `0`, or up to `1`, so that it never leaves them.
    */
  def after(familiarity: Double, answer: Answer): Double = familiarity +
    step * answer.pull *
    (if answer.pull > 0 then 1 - familiarity else familiarity)

  /**
    * The familiarity of a word after `rating`, from how well the reader knew it
    * (see [[of]]), as `saved`, or else by default, against their level,
    * `learnt`.
    */
  def rate
    (
      saved: Option[Double],
      rating: Rating,
      learnt: Option[Learnt],
    )
    : Double = after(
    of(saved, rating.level, learnt),
    rating.answer,
  )

  /**
    * Each word that the reader has rated, as `saved` holds their ratings, or
    * seen, as `seen` holds their difficulties, by the names of their keys in
    * [[words]] and [[seen]], of a language that a code names, with how well
    * they know it, against their levels in `learning` (see [[of]]).
    */
  def vocabulary
    (
      saved: Map[String, Option[Double]],
      seen: Map[String, Option[Sighting]],
      learning: List[Learnt],
    )
    : List[Lexeme] =
    (saved.collect { case (name, Some(_)) => name } ++ seen.collect {
      case (name, Some(_)) => name
    }).toList
      .distinct
      .flatMap(name =>
        lexemeOf(
          name,
          saved.get(name).flatten,
          seen.get(name).flatten,
          learning,
        ),
      )

  /**
    * The word whose keys are named `name`, of its language's code then the
    * word, with how well the reader knows it, as `saved`, or else by default,
    * from its level, as it was `seen`, against theirs in `learning`.
    */
  private def lexemeOf
    (
      name: String,
      saved: Option[Double],
      seen: Option[Sighting],
      learning: List[Learnt],
    )
    : Option[Lexeme] = name.split(":", 2) match
    case Array(code, word) => Language
        .fromCode(code)
        .map(language =>
          Lexeme(
            word,
            language,
            of(
              saved,
              seen.flatMap(_.level),
              learning.learntIn(language),
            ),
            seen,
          ),
        )
    case _ => None

  /**
    * How a word of `familiarity` is shown, unless it is among those of highest
    * priority (see [[Priority.mark]]): as its gloss while the reader is not
    * familiar with it, or else as it is, only faintly marked, with its meaning
    * on hovering, so that they may still say they did not know it.
    */
  def kind(familiarity: Double): Hint.Kind =
    if familiarity >= familiar then Hint.Kind.Known else Hint.Kind.Glossed

extension (learning: List[Learnt])

  /** The reader's level in `language`, in any variety, if they are learning it. */
  def learntIn(language: Language): Option[Learnt] =
    learning.find(_.language.alike(language))
