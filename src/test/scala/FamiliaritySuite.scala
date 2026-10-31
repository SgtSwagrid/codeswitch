package com.alecdorrington.codeswitch

import com.alecdorrington.megalingual.Language
import munit.FunSuite

class FamiliaritySuite extends FunSuite:

  /** A reader of German at B1. */
  private val b1 = Some(Learnt(Language.German, Level.B1))

  test(
    "A word not rated is taken to be known as its level for the reader puts it: little above theirs, the less the further above, and well at or below it, the better the further below, and as a level above theirs where either level is not known.",
  ):
    assertEquals(
      List(
        Level.A1,
        Level.A2,
        Level.B1,
        Level.B2,
        Level.C1,
        Level.C2,
      ).map(level => Familiarity.default(Some(level), b1)),
      List(
        0.95,
        0.9,
        Familiarity.known,
        0.5,
        0.2,
        0.2,
      ),
    )
    assertEquals(Familiarity.default(None, b1), 0.5)
    assertEquals(
      Familiarity.default(Some(Level.C1), None),
      0.5,
    )
    assertEquals(
      List(Level.B2, Level.C1).map(level =>
        Familiarity.kind(Familiarity.default(Some(level), b1)),
      ),
      List(Hint.Kind.Known, Hint.Kind.Glossed),
    )

  test(
    "A word's familiarity is its rating, if it has one, or else its default, the two alike.",
  ):
    assertEquals(
      Familiarity.of(Some(0.3), Some(Level.B2), b1),
      0.3,
    )
    assertEquals(
      Familiarity.of(None, Some(Level.B2), b1),
      0.5,
    )
    assertEquals(Familiarity.of(None, None, None), 0.5)

  test(
    "An answer moves a word's familiarity only a little, from where it was, or else from its default: down by less the better the reader knew it, not at all if mostly, and up only if well.",
  ):
    assertEqualsDouble(
      Familiarity.after(0.5, Answer.Knew),
      0.575,
      1e-9,
    )
    assertEqualsDouble(
      Familiarity.after(0.5, Answer.Barely),
      0.45,
      1e-9,
    )
    assertEqualsDouble(
      Familiarity.after(0.5, Answer.Unsure),
      0.475,
      1e-9,
    )
    assertEqualsDouble(
      Familiarity.after(0.5, Answer.Mostly),
      0.5,
      1e-9,
    )
    assertEqualsDouble(
      Familiarity.after(0.2, Answer.Unsure),
      0.19,
      1e-9,
    )
    assertEqualsDouble(
      Familiarity.after(0.9, Answer.Knew),
      0.915,
      1e-9,
    )
    assertEquals(
      Answer.values.toList.map(_.point),
      List(1, 2, 3, 4, 5),
    )
    assertEquals(
      Answer.values.toList.map(answer => math.signum(answer.pull)),
      List(-1.0, -1.0, -1.0, 0.0, 1.0),
    )
    assertEqualsDouble(
      Familiarity.after(0.5, Answer.Didnt),
      0.425,
      1e-9,
    )
    assertEqualsDouble(
      Familiarity.rate(
        None,
        Rating(
          "gewinnen",
          Language.German,
          Some(Level.B2),
          Answer.Knew,
        ),
        b1,
      ),
      Familiarity.after(0.5, Answer.Knew),
      1e-9,
    )
    assertEqualsDouble(
      Familiarity.rate(
        None,
        Rating(
          "Brücke",
          Language.German,
          Some(Level.C1),
          Answer.Didnt,
        ),
        b1,
      ),
      Familiarity.after(0.2, Answer.Didnt),
      1e-9,
    )
    assertEqualsDouble(
      Familiarity.rate(
        None,
        Rating(
          "aufstehen",
          Language.German,
          None,
          Answer.Knew,
        ),
        b1,
      ),
      Familiarity.after(0.5, Answer.Knew),
      1e-9,
    )
    assertEqualsDouble(
      Familiarity.rate(
        Some(0.7),
        Rating(
          "gewinnen",
          Language.German,
          Some(Level.C1),
          Answer.Knew,
        ),
        b1,
      ),
      Familiarity.after(0.7, Answer.Knew),
      1e-9,
    )

  test(
    "A word is kept by its whole language and its own case, apart from one spelt alike in another, or in another case, so that its every variety knows it, and is seen under the same name.",
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
    assertEquals(
      List(
        Language.German,
        Language.GermanGerman,
        Language.SwissStandardGerman,
        learnable.resolve(Language.German),
      ).map(Familiarity.key(_, "Weg").name).distinct,
      List("familiarity:de:Weg"),
    )
    assertEquals(
      Familiarity.key(Language.SwissGerman, "Weg").name,
      "familiarity:gsw:Weg",
    )
    assertEquals(
      Familiarity.key(Language.BrazilianPortuguese, "falar").name,
      Familiarity.key(Language.EuropeanPortuguese, "falar").name,
    )
    assertEquals(
      Familiarity.seenKey(Language.SwissStandardGerman, " Weg ").name,
      "seen:de:Weg",
    )

  test(
    "A word seen keeps its level and sense as last seen, or as seen before where they are not known now.",
  ):
    val hinted = Headword(
      "Bank",
      Language.German,
      "noun",
      "bank",
      "Bank",
      Some(Level.B2),
    )
    val translated = Headword(
      "Bank",
      Language.German,
      "",
      "bench",
      "bench",
      None,
    )
    val first = Familiarity.sighting(None, hinted)
    assertEquals(
      first,
      Some(Sighting(Some(Level.B2), "noun", "bank")),
    )
    assertEquals(
      Familiarity.sighting(first, translated),
      Some(Sighting(Some(Level.B2), "noun", "bench")),
    )
    assertEquals(
      Familiarity.sighting(None, translated),
      Some(Sighting(None, "", "bench")),
    )

  test(
    "Each word rated or seen is read back from the name of its key, in its whole language, with its familiarity, rated or by default from its level, and its sense as last seen, but none of a code that names no language.",
  ):
    def named(language: Language, word: String) = Familiarity
      .words
      .nameOf(Familiarity.key(language, word).name)
      .get
    val saved = Map(
      named(Language.SwissStandardGerman, "Weg") -> Some(0.9),
      named(Language.German, "Uhr: elf")         -> Some(0.4),
      named(Language.Spanish, "burro")           -> Some(0.2),
      named(Language.German, "weg")              -> None,
      "qq:Wort"                                  -> Some(0.5),
      "Wort"                                     -> Some(0.5),
    )
    val bahnhof = Sighting(Some(Level.C1), "noun", "station")
    val seen    = Map(
      named(Language.German, "Weg") ->
        Some(Sighting(Some(Level.B2), "noun", "way")),
      named(Language.German, "Bahnhof") -> Some(bahnhof),
      named(Language.German, "Gleis")   ->
        Some(Sighting(Some(Level.A2), "noun", "platform")),
      named(Language.Spanish, "perro") -> Some(Sighting(None, "noun", "dog")),
      named(Language.Italian, "casa")  ->
        Some(Sighting(Some(Level.C1), "noun", "house")),
      named(Language.German, "Haus") -> None,
      "qq:Wort"                      -> Some(Sighting(None, "", "")),
    )
    val learning = List(
      Learnt(Language.SwissStandardGerman, Level.B1),
      Learnt(Language.Spanish, Level.A2),
    )
    val vocabulary = Familiarity.vocabulary(saved, seen, learning)
    assertEquals(
      vocabulary
        .map(lexeme => (lexeme.word, lexeme.language, lexeme.familiarity))
        .toSet,
      Set(
        ("Weg", Language.German, 0.9),
        ("Uhr: elf", Language.German, 0.4),
        ("burro", Language.Spanish, 0.2),
        ("Bahnhof", Language.German, 0.2),
        ("Gleis", Language.German, 0.9),
        ("perro", Language.Spanish, 0.5),
        ("casa", Language.Italian, 0.5),
      ),
    )
    assertEquals(
      vocabulary.find(_.word == "Bahnhof").map(_.headword),
      Some(Headword(
        "Bahnhof",
        Language.German,
        "noun",
        "station",
        "station",
        Some(Level.C1),
      )),
    )
    assertEquals(
      vocabulary.find(_.word == "burro").map(_.headword),
      Some(Headword(
        "burro",
        Language.Spanish,
        "",
        "",
        "burro",
        None,
      )),
    )

  test(
    "A word not among those of highest priority is shown by its familiarity, against one threshold: as its meaning, then only faintly, a few answers on.",
  ):
    assertEquals(
      List(0.1, 0.49, 0.5, 0.8).map(Familiarity.kind),
      List(
        Hint.Kind.Glossed,
        Hint.Kind.Glossed,
        Hint.Kind.Known,
        Hint.Kind.Known,
      ),
    )
    val kinds = Iterator
      .iterate(
        Familiarity.default(Some(Level.C1), b1),
      )(Familiarity.after(_, Answer.Knew))
      .map(Familiarity.kind)
    assertEquals(kinds.indexWhere(_ == Hint.Kind.Known), 3)
