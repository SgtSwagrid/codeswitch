package com.alecdorrington.codeswitch.services

import cats.effect.{IO, SyncIO}
import cats.effect.std.Semaphore
import cats.syntax.all.*
import com.alecdorrington.codeswitch.models.{
  learntIn, Familiarity, Glanced, Headword, Learnt, Level, Lexeme, Rating,
  Sighting, Total,
}
import com.alecdorrington.megalingual.Language
import com.alecdorrington.omnibrowser.{Browser, Database}

/**
  * The one source of how well the reader knows each word, kept in the
  * extension's `databases` (see [[FamiliarityService.Kept]]): as they rated it,
  * or else as its difficulty for them puts it (see
  * [[models.Familiarity.default]]), the two alike to all that reads them, moved
  * by what they did with it on pages since (see [[models.Glance]]), and of the
  * sum of all of them, for normalising them (see [[total]]). `learning` gives
  * the languages being learnt, each with the reader's level, against which a
  * word's difficulty is measured.
  *
  * The background and the extension's pages may read, but only the background
  * writes, as it answers the `rate`, `seen` and `glance` messages, one change
  * at a time, each kept with the sum in one transaction, so that the sum, of
  * every word, is never lost to a change made in another tab. A page's content
  * script, which cannot reach the extension's IndexedDB, asks the background
  * (see [[PriorityService]]).
  */
final class FamiliarityService
  (
    databases: Browser.Databases,
    learning: IO[List[Learnt]],
  ):

  import FamiliarityService.{Kept, Known}

  /**
    * How well the reader knows each of `words`, of `language`, each with its
    * CEFR level, if known, by the word.
    */
  def of
    (
      language: Language,
      words: List[(String, Option[Level])],
    )
    : IO[Map[String, Double]] =
    val levels = words
      .map((word, level) => word.trim -> level)
      .distinctBy((word, _) => word)
    val names = levels.map((word, _) => Familiarity.nameOf(language, word))
    (
      learning,
      databases.get(Kept.ratings, names),
      databases.get(Kept.evidence, names),
    ).mapN((learning, ratings, evidence) =>
      levels
        .map((word, level) =>
          val name = Familiarity.nameOf(language, word)
          word -> Familiarity.of(
            ratings.get(name),
            level,
            learning.learntIn(language),
            evidence.get(name),
          ),
        )
        .toMap,
    )

  /**
    * Keeps the reader's `rating` of a word, in place of the evidence of what
    * they did with it before, and the sum with it. Only the background calls
    * it.
    */
  def rate(rating: Rating): IO[Unit] = writing(learning =>
    byWord(List(rating))(_.language, _.word)((name, _, known) =>
      val learnt = learning.learntIn(rating.language)
      val rated  = Familiarity.rate(
        known.saved,
        known.evidence,
        rating,
        learnt,
      )
      (
        Kept.ratings.set(name, rated) ::
          Option
            .when(known.evidence.isDefined)(Kept.evidence.remove(name))
            .toList,
        rated - Familiarity.share(
          known.saved,
          known.seen,
          known.evidence,
          learnt,
        ),
      ),
    ),
  )

  /**
    * Keeps that the reader has seen each of `shown`, with its difficulty and
    * sense, unless it is kept so already, so that a word is written only when
    * first seen, or seen otherwise (see [[models.Familiarity.sighting]]), and
    * the sum with them. Only the background calls it.
    */
  def saw(shown: List[Headword]): IO[Unit] = writing(learning =>
    byWord(shown)(_.language, _.word)((name, headwords, known) =>
      val learnt = learning.learntIn(headwords.head.language)
      val seen   = headwords.foldLeft(known.seen)(Familiarity.sighting(_, _))
      (
        seen
          .filter(_ => seen != known.seen)
          .map(Kept.sightings.set(name, _))
          .toList,
        Familiarity.share(
          known.saved,
          seen,
          known.evidence,
          learnt,
        ) - Familiarity.share(
          known.saved,
          known.seen,
          known.evidence,
          learnt,
        ),
      ),
    ),
  )

  /**
    * Keeps the evidence of each of `glances`, added to what each word had, and
    * the sum with it. Only the background calls it.
    */
  def glance(glances: List[Glanced]): IO[Unit] = writing(learning =>
    byWord(glances)(_.language, _.word)((name, glances, known) =>
      val learnt   = learning.learntIn(glances.head.language)
      val evidence = known.evidence.getOrElse(0.0) +
        glances.map(_.glance.evidence).sum
      (
        List(Kept.evidence.set(name, evidence)),
        Familiarity.share(
          known.saved,
          known.seen,
          Some(evidence),
          learnt,
        ) - Familiarity.share(
          known.saved,
          known.seen,
          known.evidence,
          learnt,
        ),
      ),
    ),
  )

  /**
    * The writes that `change` gives for each word of `items`, as `language` and
    * `word` name it, of all of one word at once, given its name and what is
    * kept of it, and the sum of how much each moved the sum, but none of a
    * blank word.
    */
  private def byWord[A]
    (items: List[A])
    (language: A => Language, word: A => String)
    (change: (String, List[A], Known) => (List[Database.Write], Double))
    : IO[(List[Database.Write], Double)] =
    val named = items
      .filter(item => word(item).trim.nonEmpty)
      .groupBy(item => Familiarity.nameOf(language(item), word(item)))
      .toList
    known(named.map(_._1)).map(known =>
      named.foldLeft((List.empty[Database.Write], 0.0)) {
        case ((writes, moved), (name, items)) =>
          val (more, by) = change(name, items, known(name))
          (writes ++ more, moved + by)
      },
    )

  /** What is kept of each word of `names`, by its name. */
  private def known(names: List[String]): IO[String => Known] = (
    databases.get(Kept.ratings, names),
    databases.get(Kept.sightings, names),
    databases.get(Kept.evidence, names),
  ).mapN((ratings, sightings, evidence) =>
    name =>
      Known(
        ratings.get(name),
        sightings.get(name),
        evidence.get(name),
      ),
  )

  /**
    * The sum of the familiarities of every word that the reader has rated or
    * seen, as kept with each change, or else counted afresh, as when their
    * levels have changed since, and so the familiarities of words not rated.
    */
  def total: IO[Double] = learning.flatMap(learning =>
    databases
      .get(Kept.total, FamiliarityService.summed)
      .flatMap(tallied(_, learning))
      .map(_.sum),
  )

  /**
    * Makes the writes that `change`, of the reader's levels in `learning`,
    * gives, one change at a time, under [[FamiliarityService.lock]], with the
    * sum, moved by as much as it gives, counted afresh first if it was kept of
    * other levels, all at once.
    */
  private def writing
    (change: List[Learnt] => IO[(List[Database.Write], Double)])
    : IO[Unit] = FamiliarityService
    .lock
    .permit
    .surround(learning.flatMap(learning =>
      databases
        .get(Kept.total, FamiliarityService.summed)
        .flatMap(kept =>
          tallied(kept, learning).flatMap(tally =>
            change(learning).flatMap((writes, moved) =>
              val next = Total(tally.sum + moved, learning)
              databases.write(
                writes ++
                  Option
                    .unless(
                      kept.contains(next),
                    )(Kept.total.set(FamiliarityService.summed, next))
                    .toList*,
              ),
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
    .fold(lexemes(learning).map(lexemes =>
      Total(
        lexemes.map(_.familiarity).sum,
        learning,
      ),
    ))(IO.pure)

  /** Every word that the reader has rated or seen, with how well they know it. */
  def all: IO[List[Lexeme]] = learning.flatMap(lexemes)

  /**
    * Every word that the reader has rated or seen, with how well they know it,
    * against their levels in `learning`.
    */
  private def lexemes(learning: List[Learnt]): IO[List[Lexeme]] = (
    databases.all(Kept.ratings),
    databases.all(Kept.sightings),
    databases.all(Kept.evidence),
  ).mapN(Familiarity.vocabulary(_, _, _, learning))

object FamiliarityService:

  /**
    * The database in which how well the reader knows each word is kept, each
    * word's values by its name (see [[models.Familiarity.nameOf]]), apart from
    * the lexicon's, as it is the reader's own.
    */
  private[codeswitch] object Kept extends Database("familiarity", version = 1):

    /** The reader's rating of each word they have rated. */
    val ratings = store[Double]("ratings")

    /**
      * The difficulty and sense of each word the reader has seen shown on a
      * page, kept apart from its rating.
      */
    val sightings = store[Sighting]("sightings")

    /**
      * How much what the reader did with each word shown since they last rated
      * it counts towards their knowing it (see [[models.Glance]]), kept apart
      * from its rating, so that it moves the word's familiarity from its
      * rating, or its default, rewriting neither, as a change of level moves
      * the default still.
      */
    val evidence = store[Double]("evidence")

    /**
      * The sum of the familiarities of every word that the reader has rated or
      * seen, under [[summed]].
      */
    val total = store[Total]("total")

  /** The key of the sum of every word's familiarity (see [[Kept.total]]). */
  private[codeswitch] val summed = "familiarity"

  /** What is kept of a word: its rating, sighting and evidence, if any. */
  private final case class Known
    (
      saved: Option[Double],
      seen: Option[Sighting],
      evidence: Option[Double],
    )

  /**
    * What lets the background write one change at a time, so that the sum kept
    * with each is never lost to another.
    */
  private lazy val lock = Semaphore.in[SyncIO, IO](1).unsafeRunSync()
