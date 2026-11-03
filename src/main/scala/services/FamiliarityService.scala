package com.alecdorrington.codeswitch.services

import cats.effect.{IO, SyncIO}
import cats.effect.std.Semaphore
import cats.syntax.all.*
import com.alecdorrington.codeswitch.models.{
  learntIn, Familiarity, Glanced, Headword, Learnt, Level, Lexeme, Rating, Total,
}
import com.alecdorrington.megalingual.Language
import com.alecdorrington.omnibrowser.Browser

/**
  * The one source of how well the reader knows each word, kept in `storage`: as
  * they rated it, or else as its difficulty for them puts it (see
  * [[models.Familiarity.default]]), the two alike to all that reads them, moved
  * by what they did with it on pages since (see [[models.Glance]]), and of the
  * sum of all of them, for normalising them (see [[total]]). `learning` gives
  * the languages being learnt, each with the reader's level, against which a
  * word's difficulty is measured.
  *
  * Any part may read, but only the background writes, as it answers the `rate`,
  * `seen` and `glance` messages, one write at a time, so that the sum, of every
  * word, kept with each write, is never lost to a write in another tab.
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
        (
          storage.get(Familiarity.key(language, word)),
          storage.get(Familiarity.evidenceKey(language, word)),
        ).parMapN((saved, evidence) =>
          word -> Familiarity.of(
            saved,
            level,
            learning.learntIn(language),
            evidence,
          ),
        ),
      )
      .map(_.toMap),
  )

  /**
    * Keeps the reader's `rating` of a word, in place of the evidence of what
    * they did with it before, and the sum with it. Only the background calls
    * it.
    */
  def rate(rating: Rating): IO[Unit] = writing(learning =>
    val learnt   = learning.learntIn(rating.language)
    val key      = Familiarity.key(rating.language, rating.word)
    val evidence = Familiarity.evidenceKey(rating.language, rating.word)
    (
      storage.get(key),
      storage.get(Familiarity.seenKey(rating.language, rating.word)),
      storage.get(evidence),
    ).flatMapN((saved, seen, before) =>
      val rated = Familiarity.rate(saved, before, rating, learnt)
      (storage.set(key, Some(rated)) *>
        storage.remove(evidence).whenA(before.isDefined)).as(
        rated - Familiarity.share(saved, seen, before, learnt),
      ),
    ),
  )

  /**
    * Keeps that the reader has seen each of `shown`, with its difficulty and
    * sense, unless it is kept so already, so that a word is written only when
    * first seen, or seen otherwise (see [[models.Familiarity.sighting]]), and
    * the sum with them. Only the background calls it.
    */
  def saw(shown: List[Headword]): IO[Unit] =
    writing(learning => byWord(shown)(_.language, _.word)(sawAll(_, learning)))

  /**
    * The sum of what `change` gives for each word of `items`, as `language` and
    * `word` name it, all of one word at once, but none of a blank word.
    */
  private def byWord[A]
    (items: List[A])
    (language: A => Language, word: A => String)
    (change: List[A] => IO[Double])
    : IO[Double] = items
    .filter(item => word(item).trim.nonEmpty)
    .groupBy(item => Familiarity.nameOf(language(item), word(item)))
    .values
    .toList
    .traverse(change)
    .map(_.sum)

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
    (
      storage.get(Familiarity.key(language, word)),
      storage.get(key),
      storage.get(Familiarity.evidenceKey(language, word)),
    ).flatMapN((saved, kept, evidence) =>
      val seen = headwords.foldLeft(kept)(Familiarity.sighting(_, _))
      storage
        .set(key, seen)
        .unlessA(seen == kept)
        .as(
          Familiarity.share(saved, seen, evidence, learnt) -
            Familiarity.share(saved, kept, evidence, learnt),
        ),
    )

  /**
    * Keeps the evidence of each of `glances`, added to what each word had, and
    * the sum with it. Only the background calls it.
    */
  def glance(glances: List[Glanced]): IO[Unit] = writing(learning =>
    byWord(glances)(_.language, _.word)(glancedAll(_, learning)),
  )

  /**
    * Keeps the evidence of `glances`, all at one word, as of the reader's
    * levels in `learning`, giving how much that changed the sum.
    */
  private def glancedAll
    (
      glances: List[Glanced],
      learning: List[Learnt],
    )
    : IO[Double] =
    val (language, word) = (glances.head.language, glances.head.word)
    val learnt           = learning.learntIn(language)
    val key              = Familiarity.evidenceKey(language, word)
    (
      storage.get(Familiarity.key(language, word)),
      storage.get(Familiarity.seenKey(language, word)),
      storage.get(key),
    ).flatMapN((saved, seen, kept) =>
      val evidence =
        Some(kept.getOrElse(0.0) + glances.map(_.glance.evidence).sum)
      storage
        .set(key, evidence)
        .as(
          Familiarity.share(saved, seen, evidence, learnt) -
            Familiarity.share(saved, seen, kept, learnt),
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
    storage.all(Familiarity.words),
    storage.all(Familiarity.seen),
    storage.all(Familiarity.evidence),
  ).mapN(Familiarity.vocabulary(_, _, _, learning))

object FamiliarityService:

  /**
    * What lets the background write one change at a time, so that the sum kept
    * with each is never lost to another.
    */
  private lazy val lock = Semaphore.in[SyncIO, IO](1).unsafeRunSync()
