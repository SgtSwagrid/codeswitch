package com.alecdorrington.codeswitch.views

import com.alecdorrington.codeswitch.models.{
  Answer, Card, Entry, Example, Familiarity, Headword, Hint, Kaikki, Level,
  Lookup, Marking, Sense, Source, Standing, Table, Wiktionary,
}
import com.alecdorrington.megalingual.Language
import munit.FunSuite

/** The cards of words, as the background makes them and a page shows them. */
class CardSuite extends FunSuite:

  test(
    "A card has its word's tables, first its principal forms but those the others repeat, the sense that fits the meaning in which it was looked up, and credits Wiktionary's page for its entry, at its whole language's section, but not a model's.",
  ):
    val anrufen = Card.of(
      Language.SwissStandardGerman,
      Kaikki.entries("anrufen").head,
      "to appeal to",
    )
    assertEquals(anrufen.fitting, Some(1))
    assertEquals(
      anrufen.tables.map(_.title),
      List("Principal forms", "Conjugation"),
    )
    assertEquals(
      anrufen.tables.head.rows.map(_.label),
      List(
        "Past participle",
        "Present participle",
        "Auxiliary",
      ),
    )
    assertEquals(anrufen.tables.head.columns, Nil)
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
      "calls",
    )
    assertEquals(made.fitting, Some(0))
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
          "",
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
    "A card has more to show if its word has more than three senses, but for the one that fits its meaning, any example, a table or an etymology, and is named, and looked up, by its word's dictionary form, part of speech and meaning.",
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
    val card = Card(entry, None, Nil, None)
    assert(!Cards.more(card))
    val four = card.copy(entry =
      entry.copy(senses = entry.senses :+ Sense(List("path"), Nil)),
    )
    assert(Cards.more(four))
    assert(!Cards.more(four.copy(fitting = Some(3))))
    assert(Cards.more(four.copy(fitting = Some(0))))
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
      Some(Level.B2),
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
    "A hint's card is of its word's dictionary form, in the sense of its meaning, beneath which it shows its tip, rated from its level, met as the page writes it, and a translation's of the translation's, in the sense of the original it stands for, beneath which it shows that, of the translation's level, met as its parts show it.",
  ):
    val hint = Hint(
      "ruft … an",
      "calls",
      "calls",
      Hint.Kind.Glossed,
      "anrufen",
      Language.German,
      "verb",
      Some(Level.C1),
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
        Some(Level.C1),
        "ruft … an",
      ),
    )
    assertEquals(
      Translated(
        List("stehe", "", "auf"),
        "get up",
        0.01,
        "aufstehen",
        Language.German,
        "verb",
        Some(Level.A2),
      ).headword,
      Headword(
        "aufstehen",
        Language.German,
        "verb",
        "get up",
        "get up",
        Some(Level.A2),
        "stehe auf",
      ),
    )

  test(
    "A cell of a table holds the form in which the reader met the word if one of its forms is the whole, its parts joined, or a word of it, in any case.",
  ):
    assert(Cards.holds("ruft an", "ruft … an"))
    assert(Cards.holds("Bank, Banke", "die Banke"))
    assert(Cards.holds("Bänke", "BÄNKE"))
    assert(Cards.holds("rief an", "  rief   an "))
    assert(!Cards.holds("ruft an", "ruft"))
    assert(!Cards.holds("rufe an", "ruft … an"))
    assert(!Cards.holds("", "ruft"))
    assert(!Cards.holds("Bank", ""))

  test(
    "A card gives how its word stands with the reader as the Vocabulary tab does, its familiarity as a percentage, its frequency as a count and its priority out of a hundred, and again once they answer.",
  ):
    val standing = Standing(0.425, 11.6, 2)
    assertEquals(
      Cards.scores(standing),
      List(
        "Familiarity" -> "43%",
        "Frequency"   -> "12",
        "Priority"    -> s"${ math.round(standing.priority * 100) }",
      ),
    )
    assertEquals(
      Cards.after(standing, Answer.Knew),
      standing.copy(familiarity = Familiarity.after(0.425, Answer.Knew)),
    )
    assert(Cards.after(standing, Answer.Knew).priority < standing.priority)
    assertEquals(
      Cards.after(standing, Answer.Mostly),
      standing,
    )
