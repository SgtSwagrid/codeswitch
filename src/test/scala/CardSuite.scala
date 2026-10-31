package com.alecdorrington.codeswitch

import com.alecdorrington.megalingual.Language
import munit.FunSuite

/** The cards of words, as the background makes them and a page shows them. */
class CardSuite extends FunSuite:

  test(
    "A card has its word's principal forms and tables, and credits Wiktionary's page for its entry, at its whole language's section, but not a model's.",
  ):
    val anrufen = Card.of(
      Language.SwissGerman,
      Kaikki.entries("anrufen").head,
    )
    assertEquals(
      anrufen.principal.map(_._1),
      List(
        "Past",
        "Past participle",
        "Present participle",
        "Auxiliary",
      ),
    )
    assertEquals(
      anrufen.tables.map(_.title),
      List("Conjugation"),
    )
    assertEquals(
      anrufen.page,
      Some("https://en.wiktionary.org/wiki/anrufen#German"),
    )
    assertEquals(
      Wiktionary.page(
        Language.Norwegian,
        "auf den Keks gehen",
      ),
      "https://en.wiktionary.org/wiki/auf_den_Keks_gehen#Norwegian_Bokm%C3%A5l",
    )
    val made = Card.of(
      Language.German,
      Kaikki.entries("anrufen").head.copy(source = Source.Model),
    )
    assertEquals(made.page, None)
    assertEquals(made.tables, anrufen.tables)

  test(
    "A card heads its word with its part of speech in full, and its genders, if it has any, as German, Russian and Polish nouns do, but not a verb.",
  ):
    assertEquals(
      List("Bank", "dom", "kot", "anrufen").map(name =>
        val card = Card.of(
          Kaikki.language(name),
          Kaikki.entries(name).head,
        )
        Cards.grammar(card.entry.pos, card.entry.genders),
      ),
      List(
        "noun, feminine",
        "noun, masculine",
        "noun, masculine",
        "verb",
      ),
    )
    assertEquals(
      Cards.grammar("noun", List("masculine", "feminine")),
      "noun, masculine or feminine",
    )
    assertEquals(Cards.grammar("adj", Nil), "adjective")
    assertEquals(
      Cards.grammar("", List("neuter")),
      "neuter",
    )
    assertEquals(Cards.grammar("", Nil), "")

  test(
    "A card has more to show if its word has more than three senses, any example, a table or an etymology, and is named, and looked up, by its word's dictionary form, part of speech and meaning.",
  ):
    val entry = Entry(
      "Weg",
      "noun",
      List("masculine"),
      List.fill(3)(Sense(List("way"), Nil)),
      Nil,
      None,
      None,
      Source.Model,
    )
    val card = Card(entry, Nil, Nil, None)
    assert(!Cards.more(card))
    assert(Cards.more(card.copy(entry =
      entry.copy(senses = Sense(List("path"), Nil) :: entry.senses),
    )))
    assert(Cards.more(card.copy(entry =
      entry.copy(senses = List(Sense(List("way"), List(Example("Weg", None))))),
    )))
    assert(Cards.more(card.copy(entry = entry.copy(etymology = Some("Old")))))
    assert(Cards.more(card.copy(tables = List(Table("Declension", Nil, Nil)))))
    val headword = Headword(
      "Weg",
      Language.German,
      "adj",
      "way, path" * 20,
      "way",
      Hint.Kind.Explained,
    )
    assertEquals(
      Cards.label(headword),
      "Weg, adjective",
    )
    assertEquals(
      Cards.label(headword.copy(pos = "")),
      "Weg",
    )
    assertEquals(
      Lookup.checked(Cards.lookup(headword)).map(_.meaning.length),
      Some(Lookup.longest),
    )

  test(
    "A hint's card is of its word's dictionary form, in the sense of its meaning, beneath which it shows its tip, and a translation's of the translation's, in the sense of the original it stands for, beneath which it shows that, rated as explained.",
  ):
    val hint = Hint(
      "ruft … an",
      "calls",
      "calls",
      Hint.Kind.Glossed,
      "anrufen",
      Language.German,
      "verb",
    )
    assertEquals(
      Marking(
        Some("calls"),
        "ruft … an",
        Hint.Kind.Glossed,
        hint,
      ).headword,
      Headword(
        "anrufen",
        Language.German,
        "verb",
        "calls",
        "ruft … an",
        Hint.Kind.Glossed,
      ),
    )
    assertEquals(
      Translated(
        List("stehe", "auf"),
        "get up",
        0.01,
        "aufstehen",
        Language.German,
        "verb",
      ).headword,
      Headword(
        "aufstehen",
        Language.German,
        "verb",
        "get up",
        "get up",
        Hint.Kind.Explained,
      ),
    )
