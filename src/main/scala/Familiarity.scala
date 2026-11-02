package com.alecdorrington.codeswitch

import cats.effect.{IO, SyncIO}
import cats.effect.std.Semaphore
import cats.syntax.all.*
import com.alecdorrington.megalingual.Language
import com.alecdorrington.omnibrowser.{Browser, StorageKey, StorageKeys}
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
  * difficulty, is `level`, if known, as a `pos` meaning `meaning` where they
  * last saw it, if known, in whose sense its card looks it up.
  */
final case class Sighting
  (
    level: Option[Level],
    pos: String,
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
  * [[FamiliarityService]]), and, if it has been seen, as what it was last seen
  * (see [[Sighting]]).
  */
final case class Lexeme
  (
    word: String,
    language: Language,
    familiarity: Double,
    seen: Option[Sighting] = None,
  ):

  /**
    * The word that its card is of, in the sense in which it was last seen, if
    * it has been, showing its meaning there.
    */
  def headword: Headword =
    val meaning = seen.fold("")(_.meaning)
    Headword(
      word,
      language,
      seen.fold("")(_.pos),
      meaning,
      if meaning.isEmpty then word else meaning,
      seen.flatMap(_.level),
    )

/**
  * The one source of how well the reader knows each word, kept in `storage`: as
  * they rated it, or else as its difficulty for them puts it (see
  * [[Familiarity.default]]), the two alike to all that reads them, and of the
  * sum of all of them, for normalising them (see [[total]]). `learning` gives
  * the languages being learnt, each with the reader's level, against which a
  * word's difficulty is measured.
  *
  * Any part may read, but only the background writes, as it answers the `rate`
  * and `seen` messages, one write at a time, so that the sum, of every word,
  * kept with each write, is never lost to a write in another tab.
  */
final class FamiliarityService
  (
    storage: Browser.Storage,
    learning: IO[List[Learnt]],
  ):

  /**
    * How well the reader knows each of `words`, of `language`, each with its
    * CEFR level, if known, by the word.
    */
  def of
    (
      language: Language,
      words: List[(String, Option[Level])],
    )
    : IO[Map[String, Double]] = learning.flatMap(learning =>
    words
      .map((word, level) => word.trim -> level)
      .distinctBy((word, _) => word)
      .parTraverse((word, level) =>
        storage
          .get(Familiarity.key(language, word))
          .map(saved =>
            word -> Familiarity.of(
              saved,
              level,
              learning.learntIn(language),
            ),
          ),
      )
      .map(_.toMap),
  )

  /**
    * Keeps the reader's `rating` of a word, and the sum with it. Only the
    * background calls it.
    */
  def rate(rating: Rating): IO[Unit] = writing(learning =>
    val learnt = learning.learntIn(rating.language)
    val key    = Familiarity.key(rating.language, rating.word)
    (
      storage.get(key),
      storage.get(Familiarity.seenKey(rating.language, rating.word)),
    ).flatMapN((saved, seen) =>
      val rated = Familiarity.rate(saved, rating, learnt)
      storage
        .set(key, Some(rated))
        .as(rated - Familiarity.share(saved, seen, learnt)),
    ),
  )

  /**
    * Keeps that the reader has seen each of `shown`, with its difficulty and
    * sense, unless it is kept so already, so that a word is written only when
    * first seen, or seen otherwise (see [[Familiarity.sighting]]), and the sum
    * with them. Only the background calls it.
    */
  def saw(shown: List[Headword]): IO[Unit] = writing(learning =>
    shown
      .filter(_.word.trim.nonEmpty)
      .groupBy(headword =>
        Familiarity.seenKey(headword.language, headword.word).name,
      )
      .values
      .toList
      .traverse(headwords => sawAll(headwords, learning))
      .map(_.sum),
  )

  /**
    * Keeps that the reader has seen `headwords`, all of one word, as of their
    * levels in `learning`, giving how much that changed the sum.
    */
  private def sawAll
    (
      headwords: List[Headword],
      learning: List[Learnt],
    )
    : IO[Double] =
    val (language, word) = (headwords.head.language, headwords.head.word)
    val learnt           = learning.learntIn(language)
    val key              = Familiarity.seenKey(language, word)
    (storage.get(Familiarity.key(language, word)), storage.get(key)).flatMapN(
      (saved, kept) =>
        val seen = headwords.foldLeft(kept)(Familiarity.sighting(_, _))
        storage
          .set(key, seen)
          .unlessA(seen == kept)
          .as(
            Familiarity.share(saved, seen, learnt) -
              Familiarity.share(saved, kept, learnt),
          ),
    )

  /**
    * The sum of the familiarities of every word that the reader has rated or
    * seen, as kept with each write, or else counted afresh, as when their
    * levels have changed since, and so the familiarities of words not rated.
    */
  def total: IO[Double] = learning.flatMap(learning =>
    storage.get(Familiarity.total).flatMap(tallied(_, learning)).map(_.sum),
  )

  /**
    * Runs `change`, of the reader's levels in `learning`, one at a time, under
    * [[FamiliarityService.lock]], and keeps the sum, moved by as much as it
    * gives, counted afresh first if it was kept of other levels.
    */
  private def writing(change: List[Learnt] => IO[Double]): IO[Unit] =
    FamiliarityService
      .lock
      .permit
      .surround(learning.flatMap(learning =>
        storage
          .get(Familiarity.total)
          .flatMap(kept =>
            tallied(kept, learning).flatMap(tally =>
              change(learning).flatMap(moved =>
                val next = Total(tally.sum + moved, learning)
                storage
                  .set(Familiarity.total, Some(next))
                  .unlessA(kept.contains(next)),
              ),
            ),
          ),
      ))

  /**
    * `kept`, if it is of the reader's levels in `learning`, or else the sum
    * counted afresh of every word.
    */
  private def tallied
    (
      kept: Option[Total],
      learning: List[Learnt],
    )
    : IO[Total] = kept
    .filter(_.learning.toSet == learning.toSet)
    .fold((storage.all(Familiarity.words), storage.all(Familiarity.seen)).mapN(
      (saved, seen) =>
        Total(
          Familiarity.vocabulary(saved, seen, learning).map(_.familiarity).sum,
          learning,
        ),
    ))(IO.pure)

  /** Every word that the reader has rated or seen, with how well they know it. */
  def all: IO[List[Lexeme]] =
    (storage.all(Familiarity.words), storage.all(Familiarity.seen), learning)
      .mapN(Familiarity.vocabulary)

object FamiliarityService:

  /**
    * What lets the background write one change at a time, so that the sum kept
    * with each is never lost to another.
    */
  private lazy val lock = Semaphore.in[SyncIO, IO](1).unsafeRunSync()

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

  /** The familiarity from which a word is known, and only faintly marked. */
  val known = 0.8

  /** The familiarity up to which a word is shown as its gloss. */
  val unknown = 0.35

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
    * seen, which [[FamiliarityService]] keeps with each write.
    */
  val total = StorageKey.local[Option[Total]]("familiarityTotal", None)

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
    * language's code then the word.
    */
  private def nameOf(language: Language, word: String): String =
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
    Some(headword.pos.trim)
      .filter(_.nonEmpty)
      .orElse(kept.map(_.pos))
      .getOrElse(""),
    Some(headword.meaning.trim)
      .filter(_.nonEmpty)
      .orElse(kept.map(_.meaning))
      .getOrElse(""),
  ))

  /**
    * The familiarity that a word the reader has not rated is taken to have,
    * from its difficulty, its CEFR `level`, if known, against the reader's
    * level in its language, `learnt`, if they are learning it: as a hint of it
    * is shown, explained a level above theirs, glossed further above, and known
    * at or below it; or else, not knowing both, as a hint explained.
    */
  def default
    (
      level: Option[Level],
      learnt: Option[Learnt],
    )
    : Double = (level, learnt)
    .mapN((level, learnt) => learnt.kind(level).getOrElse(Hint.Kind.Known))
    .fold(initial(Hint.Kind.Explained))(initial)

  /** The familiarity that a word shown as `kind` is taken to have. */
  private def initial(kind: Hint.Kind): Double = kind match
    case Hint.Kind.Explained => 0.5
    case Hint.Kind.Glossed   => 0.2
    case Hint.Kind.Known     => known

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
    * How a word of `familiarity` is shown: as its gloss while it is little
    * known, then as it is, with its meaning on hovering, and only faintly
    * marked once it is known, so that the reader may still say otherwise.
    */
  def kind(familiarity: Double): Hint.Kind =
    if familiarity >= known then Hint.Kind.Known
    else if familiarity > unknown then Hint.Kind.Explained
    else Hint.Kind.Glossed

  /** `hint`, shown as its word's familiarity in `familiarities` says. */
  def apply
    (
      familiarities: Map[String, Double],
      hint: Hint,
    )
    : Hint = familiarities
    .get(hint.word.trim)
    .fold(hint)(familiarity => hint.copy(kind = kind(familiarity)))

extension (learning: List[Learnt])

  /** The reader's level in `language`, in any variety, if they are learning it. */
  def learntIn(language: Language): Option[Learnt] =
    learning.find(_.language.alike(language))
