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
    "A card has more to show if its word has more than three senses, any example, a table or an etymology, and is named, and looked up, by its word's dictionary form, part of speech and meaning.",
  ):
    val entry = Entry(
      "Weg",
      "noun",
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
    val hint = Hint(
      "Wegen",
      "way, path" * 20,
      "way",
      Hint.Kind.Explained,
      "Weg",
      "adj",
    )
    assertEquals(Cards.label(hint), "Weg, adjective")
    assertEquals(Cards.label(hint.copy(pos = "")), "Weg")
    assertEquals(
      Lookup.checked(Cards.lookup(hint)).map(_.meaning.length),
      Some(Lookup.longest),
    )
