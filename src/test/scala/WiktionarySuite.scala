package com.alecdorrington.codeswitch

import munit.FunSuite

/** Kaikki's entries, trimmed from those it gave in October 2026. */
class WiktionarySuite extends FunSuite:

  private val url =
    "https://kaikki.org/dictionary/German/meaning/B/Ba/Bank.jsonl"

  /** `Bank`, as two words: a bench, and a bank. */
  private val bank =
    """{"word": "Bank", "pos": "noun", "etymology_number": 1, "etymology_text": "From Old High German banc.", "sounds": [{"ipa": "/baŋk/"}], "forms": [{"form": "Bank", "tags": ["genitive"]}, {"form": "Bänke", "tags": ["plural"]}, {"form": "no-table-tags", "source": "declension", "tags": ["table-tags"]}, {"form": "Bank", "tags": ["nominative", "singular"]}, {"form": "Bänke", "tags": ["definite", "nominative", "plural"]}, {"form": "Bank", "tags": ["genitive", "singular"]}, {"form": "Bänke", "tags": ["definite", "genitive", "plural"]}, {"form": "Bank", "tags": ["dative", "singular"]}, {"form": "Bänken", "tags": ["dative", "definite", "plural"]}, {"form": "Bank", "tags": ["accusative", "singular"]}, {"form": "Bänke", "tags": ["accusative", "definite", "plural"]}], "senses": [{"glosses": ["bench (which people sit on); pew"]}, {"glosses": ["workbench"]}, {"glosses": ["bank (collection of material in a body of water)"]}, {"tags": ["no-gloss"]}]}
      |{"word": "Bank", "pos": "noun", "etymology_number": 2, "forms": [{"form": "Banken", "tags": ["plural"]}], "senses": [{"glosses": ["bank (financial institution)"], "examples": [{"text": "Ich gehe zur Bank.", "english": "I am going to the bank."}]}]}
      |not json
      |""".stripMargin

  /** `gewinnen`, with some of its conjugation. */
  private val gewinnen = List(
    Form("gewann", List("past")),
    Form("gewonnen", List("participle", "past")),
    Form("haben", List("auxiliary")),
    Form(
      "gewinne",
      List(
        "first-person",
        "indicative",
        "present",
        "singular",
      ),
    ),
    Form(
      "gewinnst",
      List(
        "indicative",
        "present",
        "second-person",
        "singular",
      ),
    ),
    Form(
      "gewinnt",
      List(
        "indicative",
        "present",
        "singular",
        "third-person",
      ),
    ),
    Form(
      "gewinnen",
      List(
        "first-person",
        "indicative",
        "plural",
        "present",
      ),
    ),
    Form(
      "gewann",
      List(
        "first-person",
        "indicative",
        "preterite",
        "singular",
      ),
    ),
    Form(
      "gewannen",
      List(
        "first-person",
        "indicative",
        "plural",
        "preterite",
      ),
    ),
    Form(
      "gewänne",
      List(
        "first-person",
        "formal",
        "rare",
        "singular",
        "subjunctive",
        "subjunctive-ii",
      ),
    ),
    Form(
      "gewönne",
      List(
        "first-person",
        "formal",
        "rare",
        "singular",
        "subjunctive",
        "subjunctive-ii",
      ),
    ),
    Form(
      "habe gewonnen",
      List(
        "first-person",
        "indicative",
        "multiword-construction",
        "perfect",
        "singular",
      ),
    ),
  )

  test(
    "A word's entries are at a path of its first letter, then its first two.",
  ):
    assertEquals(
      Wiktionary.url(Language.German, "Bank"),
      url,
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

  test(
    "Each line is an entry, with its glossed senses, its forms and its sound.",
  ):
    val entries = Wiktionary.entries(bank, url)
    assertEquals(entries.size, 2)
    val bench = entries.head
    assertEquals(
      bench.senses.map(_.glosses),
      List(
        List("bench (which people sit on); pew"),
        List("workbench"),
        List("bank (collection of material in a body of water)"),
      ),
    )
    assertEquals(bench.ipa, Some("/baŋk/"))
    assertEquals(
      bench.etymology,
      Some("From Old High German banc."),
    )
    assertEquals(
      bench.source,
      Source.Wiktionary(url, translated = false),
    )
    assert(
      !bench.forms.exists(_.tags.contains("table-tags")),
      bench.forms,
    )
    assertEquals(
      entries(1).senses.head.examples,
      List(Example(
        "Ich gehe zur Bank.",
        Some("I am going to the bank."),
      )),
    )

  test("Of a word's entries, the one whose glosses fit its meaning is chosen."):
    val entries = Wiktionary.entries(bank, url)
    assertEquals(
      Wiktionary
        .choose(entries, Lookup("Bank", "noun", "bank"))
        .map(_.senses.head.glosses),
      Some(List("bank (financial institution)")),
    )
    assertEquals(
      Wiktionary
        .choose(entries, Lookup("Bank", "noun", "bench"))
        .map(_.senses.head.glosses),
      Some(List("bench (which people sit on); pew")),
    )
    assertEquals(
      Wiktionary.choose(Nil, Lookup("Bank", "noun", "bank")),
      None,
    )

  test("A noun's forms are laid out by case and number."):
    val bench = Wiktionary.entries(bank, url).head
    assertEquals(
      Forms.tables("noun", bench.forms),
      List(Table(
        "Declension",
        List("Singular", "Plural"),
        List(
          Row("Nominative", List("Bank", "Bänke")),
          Row("Genitive", List("Bank", "Bänke")),
          Row("Dative", List("Bank", "Bänken")),
          Row("Accusative", List("Bank", "Bänke")),
        ),
      )),
    )
    assertEquals(
      Forms.principal(bench.forms),
      List("Genitive" -> "Bank", "Plural" -> "Bänke"),
    )

  test(
    "A verb's forms are laid out by person and number, and by tense and mood, without those of several words.",
  ):
    val table = Forms.tables("verb", gewinnen).head
    assertEquals(
      table.columns,
      List(
        "Present",
        "Preterite",
        "Subjunctive II",
      ),
    )
    assertEquals(
      table.rows.map(row => row.label -> row.cells),
      List(
        "1st singular" -> List("gewinne", "gewann", "gewänne, gewönne"),
        "2nd singular" -> List("gewinnst", "", ""),
        "3rd singular" -> List("gewinnt", "", ""),
        "1st plural"   -> List("gewinnen", "gewannen", ""),
        "2nd plural"   -> List("", "", ""),
        "3rd plural"   -> List("", "", ""),
      ),
    )
    assertEquals(
      Forms.principal(gewinnen),
      List(
        "Past"            -> "gewann",
        "Past participle" -> "gewonnen",
        "Auxiliary"       -> "haben",
      ),
    )

  test(
    "A form's footnote marks are left out, and other parts of speech have no tables.",
  ):
    assertEquals(
      Forms.tables(
        "noun",
        List(
          Form("до́мы^*", List("nominative", "plural")),
          Form("дом", List("nominative", "singular")),
        ),
      ),
      List(Table(
        "Declension",
        List("Singular", "Plural"),
        List(Row("Nominative", List("дом", "до́мы"))),
      )),
    )
    assertEquals(Forms.tables("adj", gewinnen), Nil)
