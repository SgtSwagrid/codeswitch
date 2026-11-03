package com.alecdorrington.codeswitch.models

import com.alecdorrington.megalingual.Language
import munit.FunSuite

/** The tables of words that Kaikki gave, kept whole in the test resources. */
class FormsSuite extends FunSuite:

  private def first(name: String): Entry = Kaikki.entries(name).head

  private def table(name: String): Table =
    val entry = first(name)
    Forms
      .tables(
        Kaikki.language(name),
        entry.wordClass,
        entry.forms,
      )
      .head

  /**
    * The principal forms of `entry`, a word of `language`, that its tables do
    * not repeat, each with what it is.
    */
  private def principal
    (language: Language, entry: Entry)
    : List[(String, String)] = Forms
    .principal(language, entry.wordClass, entry.forms)
    .toList
    .flatMap(_.rows)
    .map(row => row.label -> row.cells.mkString)

  private def principal(name: String): List[(String, String)] =
    principal(Kaikki.language(name), first(name))

  /** The forms in the cells of `table`. */
  private def cells(table: Table): List[String] = table
    .rows
    .flatMap(_.cells)
    .filter(_.nonEmpty)
    .flatMap(_.split(", "))

  private def row(table: Table, label: String): List[String] = table
    .rows
    .find(_.label == label)
    .get
    .cells

  test(
    "A verb is laid out by person and number, and by tense and mood, from its own table alone.",
  ):
    val ganar = table("ganar")
    assertEquals(
      ganar.columns,
      List(
        "Present",
        "Imperfect",
        "Preterite",
        "Future",
        "Conditional",
        "Present subjunctive",
        "Imperfect subjunctive",
        "Future subjunctive",
        "Imperative",
        "Negative imperative",
      ),
    )
    assertEquals(
      ganar.rows.map(_.label),
      List(
        "1st singular",
        "2nd singular",
        "3rd singular",
        "1st plural",
        "2nd plural",
        "3rd plural",
      ),
    )
    assertEquals(
      row(ganar, "1st singular"),
      List(
        "gano",
        "ganaba",
        "gané",
        "ganaré",
        "ganaría",
        "gane",
        "ganara, ganase",
        "ganare",
        "",
        "",
      ),
    )
    assertEquals(
      row(ganar, "2nd singular").takeRight(2),
      List("gana, ganá", "ganes"),
    )
    assert(
      !cells(ganar).exists(cell =>
        cell.contains(' ') || cell.contains("ganarse") ||
        cell.contains("gánate"),
      ),
      cells(ganar),
    )
    assertEquals(
      principal("ganar"),
      List(
        "Past participle" -> "ganado",
        "Gerund"          -> "ganando",
      ),
    )

  test("A separable verb is laid out as it is in a main clause."):
    val anrufen = table("anrufen")
    assertEquals(
      anrufen.columns,
      List(
        "Present",
        "Subjunctive I",
        "Preterite",
        "Subjunctive II",
        "Imperative",
      ),
    )
    assertEquals(
      row(anrufen, "1st singular"),
      List(
        "rufe an",
        "rufe an",
        "rief an",
        "riefe an",
        "",
      ),
    )
    assert(
      !cells(anrufen).exists(_.contains("anrufe")),
      cells(anrufen),
    )
    assertEquals(
      principal("anrufen"),
      List(
        "Past participle"    -> "angerufen",
        "Present participle" -> "anrufend",
        "Auxiliary"          -> "haben",
      ),
    )

  test(
    "Compound tenses are left out beside simple ones, and a tense is named only beside another of its mood.",
  ):
    val puhua = table("puhua")
    assertEquals(
      puhua.columns,
      List(
        "Present",
        "Past",
        "Conditional",
        "Imperative",
        "Potential",
      ),
    )
    assertEquals(
      row(puhua, "3rd singular"),
      List(
        "puhuu",
        "puhui",
        "puhuisi",
        "puhukoon",
        "puhunee",
      ),
    )
    assert(
      !cells(puhua).exists(_.contains(' ')),
      cells(puhua),
    )

  test(
    "A past by gender is laid out by gender, and spellings before a reform, pronunciations, and dashes for forms there are not, are left out.",
  ):
    val govorit = table("govorit")
    assertEquals(
      govorit.columns,
      List("Present", "Imperative", "Past"),
    )
    assertEquals(
      row(govorit, "1st singular"),
      List("говорю́", "", ""),
    )
    assertEquals(
      row(govorit, "Feminine singular"),
      List("", "", "говори́ла"),
    )
    assert(
      !cells(govorit).exists(_.endsWith("ъ")),
      cells(govorit),
    )
    assertEquals(
      row(table("manger"), "1st singular").take(2),
      List("mange", "mangeais"),
    )
    assertEquals(
      row(table("talo"), "Instructive"),
      List("", "taloin"),
    )
    assertEquals(
      row(table("mangiare"), "2nd singular").takeRight(2),
      List("màngia", "mangiàre"),
    )

  test(
    "A noun is laid out by every case it has, as a Polish one by its seven, and without its possessed forms.",
  ):
    val talo = table("talo")
    assertEquals(
      talo.columns,
      List("Singular", "Plural"),
    )
    assertEquals(
      talo.rows.map(_.label),
      List(
        "Nominative",
        "Genitive",
        "Partitive",
        "Accusative",
        "Inessive",
        "Elative",
        "Illative",
        "Adessive",
        "Ablative",
        "Allative",
        "Essive",
        "Translative",
        "Instructive",
        "Abessive",
      ),
    )
    assertEquals(
      row(talo, "Genitive"),
      List("talon", "talot, talojen"),
    )
    assertEquals(
      table("Bank"),
      Table(
        "Declension",
        List("Singular", "Plural"),
        List(
          Row("Nominative", List("Bank", "Bänke")),
          Row("Genitive", List("Bank", "Bänke")),
          Row("Dative", List("Bank", "Bänken")),
          Row("Accusative", List("Bank", "Bänke")),
        ),
      ),
    )
    assertEquals(principal("Bank"), Nil)
    assertEquals(
      table("ev").rows.map(_.label),
      List(
        "Nominative",
        "Genitive",
        "Dative",
        "Accusative",
        "Locative",
        "Ablative",
      ),
    )
    val kot = table("kot")
    assertEquals(kot.columns, List("Singular", "Plural"))
    assertEquals(
      kot.rows,
      List(
        Row("Nominative", List("kot", "koty")),
        Row("Genitive", List("kota", "kotów")),
        Row("Dative", List("kotu", "kotom")),
        Row("Accusative", List("kota", "koty")),
        Row("Instrumental", List("kotem", "kotami")),
        Row("Locative", List("kocie", "kotach")),
        Row("Vocative", List("kocie", "koty")),
      ),
    )

  test(
    "A word of a language or word class whose tables are not checked has none, and only its principal forms are kept.",
  ):
    val winnen = first("winnen")
    assertEquals(
      Forms.tables(Language.Dutch, "verb", winnen.forms),
      Nil,
    )
    assert(
      winnen
        .forms
        .forall(form =>
          principal(Language.Dutch, winnen).exists(
            _._2.split(", ").contains(form.form),
          ),
        ),
      winnen.forms,
    )
    val ganar = first("ganar")
    assertEquals(
      Forms.tables(Language.Dutch, "verb", ganar.forms),
      Nil,
    )
    assertEquals(
      Forms.tables(Language.Spanish, "noun", ganar.forms),
      Nil,
    )
    assertEquals(
      Forms.tables(
        Language.LatinAmericanSpanish,
        "verb",
        ganar.forms,
      ),
      Forms.tables(Language.Spanish, "verb", ganar.forms),
    )
    val schnell = first("schnell")
    assertEquals(
      Forms.tables(
        Language.German,
        schnell.wordClass,
        schnell.forms,
      ),
      Nil,
    )
    assertEquals(
      principal("schnell"),
      List(
        "Comparative" -> "schneller",
        "Superlative" -> "am schnellsten",
      ),
    )

  test("Only the forms that a card shows are kept."):
    for name <- List(
        "gewinnen",
        "anrufen",
        "Bank",
        "schnell",
        "ganar",
        "mangiare",
        "falar",
        "manger",
        "puhua",
        "govorit",
        "talo",
        "ev",
        "dom",
        "kot",
        "winnen",
      )
    do
      val entry = first(name)
      val shown = Forms
        .tables(
          Kaikki.language(name),
          entry.wordClass,
          entry.forms,
        )
        .flatMap(cells) ++ principal(name).map(_._2).flatMap(_.split(", "))
      assert(
        entry.forms.forall(form => shown.contains(form.form)),
        s"$name: ${ entry.forms.filterNot(form => shown.contains(form.form)) }",
      )
    assert(
      first("puhua").forms.size < 40,
      first("puhua").forms.size,
    )

  test(
    "A noun's forms of several words are left out, unless all are, and so are a form's footnote marks.",
  ):
    def declension(forms: (String, String)*): List[Row] = Forms
      .tables(
        Language.German,
        "noun",
        forms
          .toList
          .map((form, kase) =>
            Form(
              form,
              List(kase, "singular"),
              Some("model"),
            ),
          ),
      )
      .flatMap(_.rows)
    assertEquals(
      declension(
        "Weg"     -> "nominative",
        "den Weg" -> "accusative",
      ),
      List(Row("Nominative", List("Weg"))),
    )
    assertEquals(
      declension(
        "der Keks" -> "nominative",
        "den Keks" -> "accusative",
      ),
      List(
        Row("Nominative", List("der Keks")),
        Row("Accusative", List("den Keks")),
      ),
    )
    assertEquals(
      Forms.tables(
        Language.Russian,
        "noun",
        List(
          Form(
            "до́мы^*",
            List("nominative", "plural"),
            Some("declension"),
          ),
          Form(
            "дом",
            List("nominative", "singular"),
            Some("declension"),
          ),
        ),
      ),
      List(Table(
        "Declension",
        List("Singular", "Plural"),
        List(Row("Nominative", List("дом", "до́мы"))),
      )),
    )

  test(
    "A principal form is left out where a table repeats it in its tense, case or number, but never a participle, and so is a form that only joins others.",
  ):
    assertEquals(
      principal(
        Language.German,
        Entry(
          "verkaufen",
          "verb",
          Nil,
          Nil,
          List(
            Form("verkaufte", List("past")),
            Form("verkauft", List("participle", "past")),
            Form("sein", List("auxiliary")),
            Form("haben", List("auxiliary")),
            Form(
              "sein or haben",
              List("auxiliary"),
              Some("conjugation"),
            ),
            Form(
              "verkauft",
              List("present", "third-person", "singular"),
              Some("conjugation"),
            ),
            Form(
              "verkaufte",
              List("preterite", "third-person", "singular"),
              Some("conjugation"),
            ),
          ),
          None,
          None,
          Source.Model,
        ),
      ),
      List(
        "Past participle" -> "verkauft",
        "Auxiliary"       -> "sein, haben",
      ),
    )
