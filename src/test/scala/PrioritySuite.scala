package com.alecdorrington.codeswitch

import com.alecdorrington.megalingual.Language
import munit.FunSuite

class PrioritySuite extends FunSuite:

  /** A hint of `word`, explained, as the model gives it. */
  private def hint(word: String): Hint = Hint(
    word,
    "meaning",
    "meaning",
    Hint.Kind.Explained,
    word,
    Language.German,
  )

  /** That a word known as well as `familiarity` was met `relative` times. */
  private def standing(familiarity: Double, relative: Double): Standing =
    Standing(familiarity, relative, relative)

  test(
    "A total counts each word met once, however often, and weighs a word against the average.",
  ):
    val total = FrequencyTotal.none.add(0, 3).add(3, 2).add(0, 1)
    assertEquals(total, FrequencyTotal(6, 2))
    assertEqualsDouble(total.average, 3, 1e-9)
    assertEqualsDouble(total.relative(6), 2, 1e-9)
    assertEqualsDouble(
      FrequencyTotal.none.relative(6),
      0,
      1e-9,
    )

  test(
    "A word that the reader is likely to know has hardly any priority, however often they meet it, so that the commonest words never crowd out the rest.",
  ):
    val article = Priority.of(50, 0.95)
    val common  = Priority.of(2, 0.5)
    val typical = Priority.of(1, 0.5)
    val rare    = Priority.of(0.1, 0.2)
    assert(article < typical / 50, article)
    assert(
      common > typical && typical > rare,
      (common, typical, rare),
    )
    assertEqualsDouble(Priority.of(5, 1), 0, 1e-9)
    assertEqualsDouble(Priority.of(0, 0.2), 0, 1e-9)

  test(
    "Meeting a word more often matters up to about as often as the average word, and little beyond.",
  ):
    assertEqualsDouble(Priority.of(1, 0), 1 - math.exp(-1), 1e-9)
    assert(Priority.of(100, 0) - Priority.of(3, 0) < 0.06)

  test(
    "The hints of the words of highest priority are highlighted, until they come to the share of the passages' words, each wherever it is.",
  ):
    val hints     = List("Bank", "Weg", "Bank", "Ufer", "Tor").map(hint)
    val standings = Map(
      "Bank" -> standing(0.5, 2),
      "Weg"  -> standing(0.5, 1),
      "Ufer" -> standing(0.2, 0.1),
      "Tor"  -> standing(0.9, 5),
    )
    assertEquals(
      Priority.highlighted(hints, standings, 40),
      Set("Bank"),
    )
    assertEquals(
      Priority.highlighted(hints, standings, 60),
      Set("Bank", "Weg"),
    )
    assertEquals(
      Priority.highlighted(hints, standings, 8),
      Set.empty,
    )

  test("A word of no priority is never highlighted."):
    assertEquals(
      Priority.highlighted(
        List(hint("Bank")),
        Map("Bank" -> standing(1, 5)),
        1000,
      ),
      Set.empty,
    )

  test(
    "Each hint not highlighted is shown as its familiarity says: faintly marked if the reader is familiar with it, or else as its gloss.",
  ):
    val hints     = List("Bank", "Weg", "Ufer").map(hint)
    val standings = Map(
      "Bank" -> standing(0.5, 2),
      "Weg"  -> standing(0.5, 0.5),
      "Ufer" -> standing(0.2, 0.1),
    )
    val highlighted = Priority.highlighted(hints, standings, 20)
    assertEquals(
      hints.map(Priority.mark(_, standings, highlighted).kind),
      List(
        Hint.Kind.Explained,
        Hint.Kind.Known,
        Hint.Kind.Glossed,
      ),
    )

  test(
    "A word is likelier to be switched the higher the priority of the word it was switched into before, within bounds, and as likely as a typical word if it never was.",
  ):
    val typical = Priority.of(1, 0.5)
    assertEqualsDouble(Priority.chance(0.05, None), 0.05, 1e-9)
    assertEqualsDouble(
      Priority.chance(0.05, Some(typical * 2)),
      0.1,
      1e-9,
    )
    assertEqualsDouble(
      Priority.chance(0.05, Some(typical * 10)),
      0.2,
      1e-9,
    )
    assertEqualsDouble(
      Priority.chance(0.05, Some(0)),
      0.005,
      1e-9,
    )
    assertEqualsDouble(
      Priority.chance(0.4, Some(typical * 10)),
      1,
      1e-9,
    )
    assertEqualsDouble(
      Priority.draw(0.1, 0, Some(typical * 2)),
      0.05,
      1e-9,
    )
    assertEqualsDouble(Priority.draw(0.1, 0, None), 0.1, 1e-9)

  test(
    "As the intensity rises, a word not switched before is drawn again from where it was not, so that as many are switched as if they were drawn afresh, whether likelier than a typical word or less.",
  ):
    val typical            = Priority.of(1, 0.5)
    val (floor, intensity) = (0.05, 0.1)
    List(4.0, 0.5, 0.25).foreach(likelier =>
      val priority = Some(typical * likelier)
      // Its draw had been from 0 up to 1 / likelier, and not below the floor,
      // so the rise switches this share of those not switched before: a
      // quarter, for one four times likelier.
      val share = (intensity - floor) / (1 / likelier - floor)
      assert(share > 0 && share < 1, share)
      assertEqualsDouble(
        Priority.draw(floor, floor, priority),
        floor,
        1e-9,
      )
      assertEqualsDouble(
        Priority.draw(1, floor, priority),
        1 / likelier,
        1e-9,
      )
      assertEqualsDouble(
        Priority.draw(floor + (1 - floor) * share, floor, priority),
        intensity,
        1e-9,
      ),
    )
    assertEqualsDouble(
      (intensity - floor) / (1.0 / 4 - floor),
      0.25,
      1e-9,
    )

  test(
    "A word switched stands for one over its chance of being switched into its language, each of those being learnt as likely.",
  ):
    val typical = Priority.of(1, 0.5)
    assertEqualsDouble(
      Priority.times(0.05, Some(typical * 4), 1),
      5,
      1e-9,
    )
    assertEqualsDouble(Priority.times(0.4, None, 2), 5, 1e-9)
    assertEqualsDouble(
      Priority.times(0.4, Some(typical * 10), 3),
      3,
      1e-9,
    )
