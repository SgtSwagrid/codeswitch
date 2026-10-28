package com.alecdorrington.codeswitch

import com.alecdorrington.megalingual.Language
import munit.FunSuite

/** Kaikki's entries, as it gave them in October 2026. */
class WiktionarySuite extends FunSuite:

  test(
    "A word's entries are at a path of its first letter, then its first two, of its name composed, with its dots and slashes written as Kaikki writes them.",
  ):
    assertEquals(
      Wiktionary.url(Language.German, "Bank"),
      "https://kaikki.org/dictionary/German/meaning/B/Ba/Bank.jsonl",
    )
    assertEquals(
      Wiktionary.url(Language.German, "Brücke"),
      "https://kaikki.org/dictionary/German/meaning/B/Br/Br%C3%BCcke.jsonl",
    )
    assertEquals(
      Wiktionary.url(Language.French, "a"),
      "https://kaikki.org/dictionary/French/meaning/a/a/a.jsonl",
    )
    assertEquals(
      Wiktionary.url(
        Language.Norwegian,
        "auf den Keks gehen",
      ),
      "https://kaikki.org/dictionary/Norwegian%20Bokm%C3%A5l/meaning/a/au/auf%20den%20Keks%20gehen.jsonl",
    )
    assertEquals(
      Wiktionary.url(Language.German, "z. B."),
      "https://kaikki.org/dictionary/German/meaning/z/z_/z_dot_%20B_dot_.jsonl",
    )
    assertEquals(
      Wiktionary.url(Language.German, "und/oder"),
      "https://kaikki.org/dictionary/German/meaning/u/un/und_slash_oder.jsonl",
    )
    assertEquals(
      Wiktionary.url(Language.German, "Brücke"),
      Wiktionary.url(Language.German, "Brücke"),
    )

  test(
    "A variety's words are looked up in its whole language's dictionary, as Kaikki names it, which it has of every language.",
  ):
    assertEquals(
      Wiktionary.url(Language.SwissGerman, "Bank"),
      Wiktionary.url(Language.German, "Bank"),
    )
    assertEquals(
      Wiktionary.nameOf(Language.BrazilianPortuguese),
      "Portuguese",
    )
    assertEquals(
      Wiktionary.nameOf(Language.Croatian),
      "Serbo-Croatian",
    )
    assertEquals(
      Wiktionary.nameOf(Language.Filipino),
      "Tagalog",
    )
    assertEquals(
      Language.roots.map(Wiktionary.nameOf).filterNot(Kaikki.languages),
      Nil,
    )

  test(
    "Each line is an entry, with its glossed senses, its examples, its forms, its sound and where it came from, but lines that are not entries, and senses with no glosses, are left out.",
  ):
    val url     = "https://kaikki.org/Bank"
    val entries = Kaikki.entries("Bank")
    assertEquals(
      entries.map(_.pos),
      List("noun", "noun"),
    )
    val bench = entries.head
    assertEquals(
      bench.senses.map(_.glosses),
      List(
        List("bench (which people sit on); pew"),
        List("workbench (which things can be set down on)"),
        List("bank (collection of material in a body of water)"),
        List("substitutes' bench"),
      ),
    )
    assertEquals(bench.ipa, Some("/baŋk/"))
    assert(
      bench.etymology.exists(_.startsWith("From Middle High German")),
      bench.etymology,
    )
    assertEquals(
      bench.source,
      Source.Wiktionary(url, translated = false),
    )
    assert(
      !bench
        .forms
        .exists(_.tags.exists(Set("table-tags", "inflection-template"))),
      bench.forms,
    )
    assert(
      Kaikki
        .entries("anrufen")
        .head
        .senses
        .flatMap(_.examples)
        .contains(Example(
          "Ich habe ihn gerade angerufen.",
          Some("I've just called him."),
        )),
    )
    assertEquals(
      Wiktionary
        .entries(
          Language.German,
          """{"word": "Bank", "pos": "noun", "senses": [{"tags": ["no-gloss"]}, {"glosses": ["bank"]}]}
            |not json
            |{"pos": "noun"}
            |""".stripMargin,
          "https://kaikki.org/Bank",
        )
        .map(_.senses.map(_.glosses)),
      List(List(List("bank"))),
    )

  test(
    "Of a word's entries, those of its part of speech with senses may be its own, or all with senses if none is of it, and the one whose glosses fit its meaning is chosen.",
  ):
    val entries = Kaikki.entries("Bank")
    val found   = Wiktionary.candidates(entries, Lookup("Bank", "noun", "bank"))
    assertEquals(found.map(_._2), List(0, 1))
    assertEquals(
      Wiktionary.choose(found.map(_._1), "bank").map(_.senses.head.glosses),
      Some(List("bank (financial institution)")),
    )
    assertEquals(
      Wiktionary.choose(found.map(_._1), "bench").map(_.senses.head.glosses),
      Some(List("bench (which people sit on); pew")),
    )
    assertEquals(Wiktionary.choose(Nil, "bank"), None)
    val manger = Kaikki.entries("manger")
    assertEquals(
      Wiktionary
        .candidates(
          manger,
          Lookup("manger", "adv", "to eat"),
        )
        .map(_._1.pos),
      manger.map(_.pos),
    )
    assertEquals(
      Wiktionary.candidates(
        manger.map(_.copy(senses = Nil)),
        Lookup("manger", "verb", "to eat"),
      ),
      Nil,
    )
