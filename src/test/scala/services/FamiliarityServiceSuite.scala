package com.alecdorrington.codeswitch.services

import cats.effect.IO
import com.alecdorrington.codeswitch.models.{
  Familiarity, Glance, Glanced, Headword, Learnt, Level,
}
import com.alecdorrington.megalingual.Language
import munit.CatsEffectSuite

class FamiliarityServiceSuite extends CatsEffectSuite:

  /** German, being learnt. */
  private val german = Language.German

  test(
    "What the reader did with a word moves its familiarity, as every part reads it, from its default, which a change of level still moves.",
  ):
    for
      storage  <- Memory()
      learning <- IO.ref(List(Learnt(german, Level.B1)))
      service = FamiliarityService(storage, learning.get)
      _ <- service.saw(List(Headword(
        "Bank",
        german,
        "noun",
        "bank",
        "bank",
        Some(Level.B2),
      )))
      _ <- service.glance(List.fill(3)(Glanced("Bank", german, Glance.Passed)))
      before  <- service.of(german, List("Bank" -> Some(Level.B2)))
      lexemes <- service.all
      total   <- service.total
      _       <- learning.set(List(Learnt(german, Level.B2)))
      after   <- service.of(german, List("Bank" -> Some(Level.B2)))
    yield
      val moved = Familiarity.weighed(0.5, 3 * Familiarity.passing)
      assertEqualsDouble(
        before.getOrElse("Bank", -1.0),
        moved,
        1e-12,
      )
      assertEqualsDouble(
        lexemes.find(_.word == "Bank").fold(-1.0)(_.familiarity),
        moved,
        1e-12,
      )
      assertEqualsDouble(total, moved, 1e-12)
      assertEqualsDouble(
        after.getOrElse("Bank", -1.0),
        Familiarity.weighed(
          Familiarity.known,
          3 * Familiarity.passing,
        ),
        1e-12,
      )
