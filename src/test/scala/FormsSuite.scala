package com.alecdorrington.codeswitch

import munit.FunSuite

/** The tables of words that Kaikki gave, kept whole in the test resources. */
class FormsSuite extends FunSuite:

  private def first(name: String): Entry = Kaikki.entries(name).head

  private def table(name: String): Table =
    val entry = first(name)
    Forms.tables(entry.pos, entry.forms).head

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
        "1st plural",
        "2nd singular",
        "2nd plural",
        "3rd singular",
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
      Forms.principal(first("ganar").forms),
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
      Forms.principal(first("anrufen").forms),
      List(
        "Past"               -> "rief an",
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
    "A past by gender is laid out by gender, and spellings before a reform are left out.",
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

  test("Pronunciations, and dashes for forms there are not, are left out."):
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
    "A noun is laid out by every case it has, and without its possessed forms.",
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
    assertEquals(
      Forms.principal(first("Bank").forms),
      List("Genitive" -> "Bank", "Plural" -> "Bänke"),
    )
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

  test("Other parts of speech have no tables, but their principal forms."):
    val schnell = first("schnell")
    assertEquals(
      Forms.tables(schnell.pos, schnell.forms),
      Nil,
    )
    assertEquals(
      Forms.principal(schnell.forms),
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
      )
    do
      val entry = first(name)
      val shown = Forms.tables(entry.pos, entry.forms).flatMap(cells) ++
        Forms.principal(entry.forms).map(_._2).flatMap(_.split(", "))
      assert(
        entry.forms.forall(form => shown.contains(form.form)),
        s"$name: ${ entry.forms.filterNot(form => shown.contains(form.form)) }",
      )
    assert(
      first("puhua").forms.size < 40,
      first("puhua").forms.size,
    )

  test("A noun's forms of several words are left out, unless all are."):
    def declension(forms: (String, String)*): List[Row] = Forms
      .tables(
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

  test("A form's footnote marks are left out."):
    assertEquals(
      Forms.tables(
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
