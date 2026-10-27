package com.alecdorrington.codeswitch

import com.alecdorrington.megalingual.Language
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

  test("A word not yet rated begins where its kind puts it."):
    assertEqualsDouble(
      Familiarity.rate(
        None,
        Rating(
          "gewinnen",
          Hint.Kind.Explained,
          Answer.Knew,
        ),
      ),
      Familiarity.after(0.5, Answer.Knew),
      1e-9,
    )
    assertEqualsDouble(
      Familiarity.rate(
        None,
        Rating(
          "Brücke",
          Hint.Kind.Glossed,
          Answer.Didnt,
        ),
      ),
      Familiarity.after(0.2, Answer.Didnt),
      1e-9,
    )

  test("A word already rated moves on from where it was."):
    assertEqualsDouble(
      Familiarity.rate(
        Some(0.7),
        Rating(
          "gewinnen",
          Hint.Kind.Glossed,
          Answer.Knew,
        ),
      ),
      Familiarity.after(0.7, Answer.Knew),
      1e-9,
    )

  test(
    "A word is kept by its language and its own case, apart from one spelt alike in another, or in another case.",
  ):
    val names = List(
      Familiarity.key(Language.German, "Weg"),
      Familiarity.key(Language.German, "weg"),
      Familiarity.key(Language.Italian, "burro"),
      Familiarity.key(Language.Spanish, "burro"),
      Familiarity.key(Language.German, " Weg "),
    ).map(_.name)
    assertEquals(
      names,
      List(
        "familiarity:de:Weg",
        "familiarity:de:weg",
        "familiarity:it:burro",
        "familiarity:es:burro",
        "familiarity:de:Weg",
      ),
    )

  test(
    "A word is kept by its whole language, so that its every variety knows it, and one stored as a whole is rated and read alike.",
  ):
    assertEquals(
      List(
        Language.German,
        Language.GermanGerman,
        Language.SwissGerman,
        learnable.resolve(Language.German),
      ).map(Familiarity.key(_, "Weg").name).distinct,
      List("familiarity:de:Weg"),
    )
    assertEquals(
      Familiarity.key(Language.BrazilianPortuguese, "falar").name,
      Familiarity.key(Language.EuropeanPortuguese, "falar").name,
    )

  test(
    "It takes several answers to move a word from one way of showing to the next.",
  ):
    val kinds = Iterator
      .iterate(
        Familiarity.initial(Hint.Kind.Explained),
      )(Familiarity.after(_, Answer.Knew))
      .map(Familiarity.kind)
    assertEquals(
      kinds.indexWhere(_ == Hint.Kind.Known),
      6,
    )

  test(
    "A word is shown by its familiarity: as its meaning, then as it is, then only faintly.",
  ):
    assertEquals(
      List(0.1, 0.35, 0.36, 0.79, 0.8).map(Familiarity.kind),
      List(
        Hint.Kind.Glossed,
        Hint.Kind.Glossed,
        Hint.Kind.Explained,
        Hint.Kind.Explained,
        Hint.Kind.Known,
      ),
    )

  test(
    "A word the reader has rated is shown as they know it, and others as their level says.",
  ):
    val known = Map("gewinnen" -> 0.9, "Ausschreibung" -> 0.5)
    assertEquals(
      Familiarity(known, explained),
      explained.copy(kind = Hint.Kind.Known),
    )
    assertEquals(
      Familiarity(known, glossed),
      glossed.copy(kind = Hint.Kind.Explained),
    )
    assertEquals(Familiarity(Map.empty, glossed), glossed)
