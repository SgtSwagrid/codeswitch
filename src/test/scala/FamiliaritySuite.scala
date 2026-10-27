package com.alecdorrington.codeswitch

import munit.FunSuite

class FamiliaritySuite extends FunSuite:

  private val explained = Hint(
    "gewonnen",
    "won",
    Hint.Kind.Explained,
    "gewinnen",
  )

  private val glossed = Hint(
    "Ausschreibung",
    "tender",
    Hint.Kind.Glossed,
    "Ausschreibung",
  )

  test("An answer moves a word's familiarity only a little towards it."):
    assertEqualsDouble(
      Familiarity.after(0.5, Answer.Knew),
      0.575,
      1e-9,
    )
    assertEqualsDouble(
      Familiarity.after(0.5, Answer.Unsure),
      0.515,
      1e-9,
    )
    assertEqualsDouble(
      Familiarity.after(0.5, Answer.Didnt),
      0.425,
      1e-9,
    )

  test("A word not yet rated begins where its level puts it."):
    assertEquals(
      Familiarity.rate(
        Map.empty,
        "gewinnen",
        Hint.Kind.Explained,
        Answer.Knew,
      ),
      Map("gewinnen" -> Familiarity.after(0.5, Answer.Knew)),
    )
    assertEquals(
      Familiarity.rate(
        Map.empty,
        "Brücke",
        Hint.Kind.Glossed,
        Answer.Didnt,
      ),
      Map("brücke" -> Familiarity.after(0.2, Answer.Didnt)),
    )

  test(
    "A word already rated moves on from where it was, however it is written.",
  ):
    assertEquals(
      Familiarity.rate(
        Map("gewinnen" -> 0.7),
        " Gewinnen ",
        Hint.Kind.Glossed,
        Answer.Knew,
      ),
      Map("gewinnen" -> Familiarity.after(0.7, Answer.Knew)),
    )

  test(
    "It takes several answers to move a word from one way of showing to the next.",
  ):
    val answers = Iterator
      .iterate(
        Familiarity.initial(Hint.Kind.Explained),
      )(Familiarity.after(_, Answer.Knew))
      .map(Familiarity.kind)
    assertEquals(answers.indexWhere(_.isEmpty), 6)

  test(
    "A word is shown by its familiarity: as its meaning, then as it is, then not at all.",
  ):
    assertEquals(
      List(0.1, 0.35, 0.36, 0.79, 0.8).map(Familiarity.kind),
      List(
        Some(Hint.Kind.Glossed),
        Some(Hint.Kind.Glossed),
        Some(Hint.Kind.Explained),
        Some(Hint.Kind.Explained),
        None,
      ),
    )

  test(
    "A word the reader has rated is shown as they know it, and others as their level says.",
  ):
    val known = Map("gewinnen" -> 0.9, "ausschreibung" -> 0.5)
    assertEquals(Familiarity(known, explained), None)
    assertEquals(
      Familiarity(known, glossed),
      Some(glossed.copy(kind = Hint.Kind.Explained)),
    )
    assertEquals(
      Familiarity(Map.empty, glossed),
      Some(glossed),
    )
