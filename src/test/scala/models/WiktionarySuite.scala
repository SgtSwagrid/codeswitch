package com.alecdorrington.codeswitch.models

import com.alecdorrington.megalingual.Language
import io.circe.Json
import io.circe.syntax.*
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
      Wiktionary.url(Language.SwissStandardGerman, "Bank"),
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
      Wiktionary.url(Language.SwissGerman, "Chuchi"),
      "https://kaikki.org/dictionary/Alemannic%20German/meaning/C/Ch/Chuchi.jsonl",
    )
    assertEquals(
      Language
        .values
        .toList
        .map(_.whole)
        .distinct
        .map(Wiktionary.nameOf)
        .filterNot(Kaikki.languages),
      Nil,
    )

  test(
    "Each line is an entry, with its glossed senses, its examples, its forms, its sound and where it came from, but lines that are not entries, and senses with no glosses, are left out.",
  ):
    val url     = "https://kaikki.org/Bank"
    val entries = Kaikki.entries("Bank")
    assertEquals(
      entries.map(_.wordClass),
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
    "An entry keeps its recordings on Wikimedia Commons, as MP3s, each with its tags, but only those that a reader of some variety of its language hears, and none from elsewhere.",
  ):
    assertEquals(
      Kaikki.entries("Bank").head.recordings,
      List(
        Recording(
          "De-Bank.ogg",
          "https://upload.wikimedia.org/wikipedia/commons/transcoded/2/2d/De-Bank.ogg/De-Bank.ogg.mp3",
          Nil,
        ),
        Recording(
          "De-Bank2.ogg",
          "https://upload.wikimedia.org/wikipedia/commons/transcoded/3/3b/De-Bank2.ogg/De-Bank2.ogg.mp3",
          List("Germany", "Berlin"),
        ),
      ),
    )
    assertEquals(
      List("manger", "dom", "anrufen", "falar").map(
        Kaikki.entries(_).head.recordings.map(_.file),
      ),
      List(
        List("Fr-manger.ogg"),
        List("Ru-дом.ogg"),
        List("De-anrufen.ogg"),
        Nil,
      ),
    )
    assertEquals(
      Wiktionary
        .entries(
          Language.German,
          """{"word": "Weg", "pos": "noun", "senses": [{"glosses": ["way"]}], "sounds": [{"ipa": "/veːk/"}, {"audio": "De-Weg.ogg", "mp3_url": "https://example.com/De-Weg.ogg.mp3"}, {"audio": "De-Weg2.ogg", "ogg_url": "https://upload.wikimedia.org/wikipedia/commons/a/ab/De-Weg2.ogg"}, {"mp3_url": "https://upload.wikimedia.org/wikipedia/commons/transcoded/a/ab/De-Weg3.ogg/De-Weg3.ogg.mp3"}]}""",
          "https://kaikki.org/Weg",
        )
        .map(_.recordings),
      List(Nil),
    )
    assert(
      !Wiktionary.recorded("https://upload.wikimedia.org.example.com/x.mp3"),
    )
    assert(!Wiktionary.recorded("https://kaikki.org/wikipedia/commons/x.mp3"))

  /** A recording of `file` said by a speaker of whom Wiktionary says `tags`. */
  private def recording(file: String, tags: String*): Recording = Recording(
    file,
    s"https://upload.wikimedia.org/wikipedia/commons/transcoded/0/00/$file/$file.mp3",
    tags.toList,
  )

  test(
    "A reader hears the first recording by a speaker from where their variety is spoken, a broader language's first variety, or else the first by a speaker of whom Wiktionary says nothing, or else the first.",
  ):
    def heard(language: Language, name: String): Option[String] = Wiktionary
      .heard(
        language,
        Kaikki.entries(name).head.recordings,
      )
      .map(_.file)
    assertEquals(
      List(
        Language.German,
        Language.GermanGerman,
        Language.SwissStandardGerman,
      ).map(heard(_, "Bank")),
      List(
        Some("De-Bank2.ogg"),
        Some("De-Bank2.ogg"),
        Some("De-Bank.ogg"),
      ),
    )
    assertEquals(
      List(
        Language.German,
        Language.SwissStandardGerman,
      ).map(heard(_, "gewinnen")),
      List(
        Some("De-gewinnen2.ogg"),
        Some("De-gewinnen.ogg"),
      ),
    )
    assertEquals(
      heard(
        Language.SwissStandardGerman,
        "schnell",
      ),
      Some("De-schnell.ogg"),
    )
    val water = List(
      recording(
        "En-uk-water.ogg",
        "Received-Pronunciation",
      ),
      recording(
        "LL-Q1860 (eng)-Back ache-water.wav",
        "UK",
      ),
      recording(
        "LL-Q1860 (eng)-Nail123Real-water.wav",
        "US",
      ),
      recording("En-us-water.ogg", "US"),
      recording(
        "LL-Q1860 (eng)-Byrhtnoð-water.wav",
        "Philadelphia",
      ),
    )
    assertEquals(
      List(
        Language.English,
        Language.BritishEnglish,
        Language.AmericanEnglish,
        Language.AustralianEnglish,
      ).map(Wiktionary.heard(_, water).map(_.file)),
      List(
        Some("En-uk-water.ogg"),
        Some("En-uk-water.ogg"),
        Some("LL-Q1860 (eng)-Nail123Real-water.wav"),
        Some("En-uk-water.ogg"),
      ),
    )
    val casa = List(
      recording(
        "LL-Q5146 (por)-Jjunho-casa.wav",
        "Brazil",
      ),
      recording(
        "pt-pt-casa.ogg",
        "Northern",
        "Portugal",
      ),
      recording("casa.ogg"),
    )
    assertEquals(
      List(
        Language.Portuguese,
        Language.BrazilianPortuguese,
        Language.EuropeanPortuguese,
      ).map(Wiktionary.heard(_, casa).map(_.file)),
      List(
        Some("LL-Q5146 (por)-Jjunho-casa.wav"),
        Some("LL-Q5146 (por)-Jjunho-casa.wav"),
        Some("pt-pt-casa.ogg"),
      ),
    )
    assertEquals(
      Wiktionary.heard(Language.German, Nil),
      None,
    )

  test(
    "A reader of Chinese, whose entries are of several lects, hears only a recording of their own, and an entry keeps only those.",
  ):
    val sounds = List(
      recording("Zh nǐ hǎo.ogg", "Mandarin", "Standard"),
      recording(
        "zh-yue-你好.opus",
        "Cantonese",
        "Jyutping",
      ),
      recording(
        "Cdo hello.ogg",
        "Min-Dong",
        "Foochow-Romanized",
      ),
      recording("hello.ogg"),
    )
    assertEquals(
      List(
        Language.SimplifiedChinese,
        Language.TraditionalChinese,
        Language.TaiwaneseChinese,
        Language.Cantonese,
      ).map(Wiktionary.heard(_, sounds).map(_.file)),
      List(
        Some("Zh nǐ hǎo.ogg"),
        Some("Zh nǐ hǎo.ogg"),
        Some("Zh nǐ hǎo.ogg"),
        Some("zh-yue-你好.opus"),
      ),
    )
    assertEquals(
      Wiktionary
        .heard(
          Language.SimplifiedChinese,
          sounds.drop(1),
        )
        .map(_.file),
      None,
    )
    val line = Json.obj(
      "word"   -> "你好".asJson,
      "pos"    -> "intj".asJson,
      "senses" -> Json.arr(Json.obj("glosses" -> List("hello").asJson)),
      "sounds" ->
        sounds
          .map(sound =>
            Json.obj(
              "audio"   -> sound.file.asJson,
              "mp3_url" -> sound.url.asJson,
              "tags"    -> sound.tags.asJson,
            ),
          )
          .asJson,
    )
    assertEquals(
      Wiktionary
        .entries(
          Language.SimplifiedChinese,
          line.noSpaces,
          "https://kaikki.org/你好",
        )
        .map(_.recordings.map(_.file)),
      List(List("Zh nǐ hǎo.ogg", "zh-yue-你好.opus")),
    )

  test(
    "An entry's genders are those that all its glossed senses are tagged with, in a fixed order, which German, Russian and Polish nouns have, but a verb has none of.",
  ):
    assertEquals(
      List(
        "Bank",
        "dom",
        "kot",
        "falar",
        "anrufen",
        "govorit",
      ).map(Kaikki.entries(_).map(_.genders)),
      List(
        List(List("feminine"), List("feminine")),
        List(List("masculine")),
        List(List("masculine")),
        List(Nil, List("masculine")),
        List(Nil),
        List(Nil),
      ),
    )
    assertEquals(
      Wiktionary
        .entries(
          Language.Spanish,
          """{"word": "mar", "pos": "noun", "senses": [{"glosses": ["sea"], "tags": ["feminine", "masculine"]}, {"glosses": ["a lot"], "tags": ["masculine", "feminine", "figuratively"]}, {"tags": ["no-gloss"]}]}
            |{"word": "x", "pos": "noun", "senses": [{"glosses": ["a"], "tags": ["masculine"]}, {"glosses": ["b"], "tags": ["neuter"]}]}
            |{"word": "y", "pos": "noun", "senses": []}
            |""".stripMargin,
          "https://kaikki.org/mar",
        )
        .map(_.genders),
      List(List("masculine", "feminine"), Nil, Nil),
    )

  test(
    "Of a word's entries, those of its word class with senses may be its own, or all with senses if none is of it, and the one whose glosses fit its meaning is chosen.",
  ):
    val entries = Kaikki.entries("Bank")
    val found   = Wiktionary.candidates(
      entries,
      Lookup("Bank", "noun", "bank", Language.German),
    )
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
          Lookup(
            "manger",
            "adv",
            "to eat",
            Language.French,
          ),
        )
        .map(_._1.wordClass),
      manger.map(_.wordClass),
    )
    assertEquals(
      Wiktionary.candidates(
        manger.map(_.copy(senses = Nil)),
        Lookup(
          "manger",
          "verb",
          "to eat",
          Language.French,
        ),
      ),
      Nil,
    )

  test(
    "Of a word's senses, the one with a part of a gloss closest to a part of its meaning fits it, by its words of more than two letters, unless a part has no others, in any of their forms that begin with one, but those in brackets and those every part has, or the first of those as close, but none of one sense, nor if none is close.",
  ):
    def fitting
      (
        name: String,
        meaning: String,
        entry: Int = 0,
      )
      : Option[Int] = Wiktionary.fitting(Kaikki.entries(name)(entry), meaning)
    assertEquals(
      List(
        "to win",
        "to persuade",
        "persuaded",
        "to",
        "a cat",
        "",
      ).map(fitting("gewinnen", _)),
      List(
        Some(0),
        Some(2),
        Some(2),
        None,
        None,
        None,
      ),
    )
    assertEquals(
      List("to beat", "earned").map(fitting("ganar", _)),
      List(Some(3), Some(1)),
    )
    assertEquals(
      List(
        "to call (on the phone)",
        "calls",
        "to appeal to",
      ).map(fitting("anrufen", _)),
      List(Some(0), Some(0), Some(1)),
    )
    assertEquals(
      List("the company", "household", "houses").map(fitting("talo", _)),
      List(Some(4), Some(3), Some(0)),
    )
    assertEquals(
      List("cat", "tomcat").map(fitting("kot", _)),
      List(Some(0), Some(1)),
    )
    assertEquals(
      List(
        "a bank",
        "a vault, a storage facility",
      ).map(fitting("Bank", _, 1)),
      List(Some(0), Some(1)),
    )
    assertEquals(fitting("schnell", "quick"), None)
