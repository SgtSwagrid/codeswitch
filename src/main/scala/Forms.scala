package com.alecdorrington.codeswitch

/**
  * A table of a word's forms, called `title`, with `columns`, and `rows`, each
  * with a cell under each column, empty where the word has no such form.
  */
final case class Table
  (
    title: String,
    columns: List[String],
    rows: List[Row],
  )

/** A row of a [[Table]], called `label`, with its `cells`. */
final case class Row(label: String, cells: List[String])

/**
  * Lays a word's forms out for the card, by the tags that say what each is, in
  * the same way for every language, as Wiktionary tags them alike.
  */
object Forms:

  /** Person and number, the rows of a verb's table, in their order. */
  private val persons = List(
    "first-person singular",
    "second-person singular",
    "third-person singular",
    "first-person plural",
    "second-person plural",
    "third-person plural",
  ).map(_.split(' ').toSet)

  /** Cases, the rows of a noun's table, in their order. */
  private val cases = List(
    "nominative",
    "genitive",
    "dative",
    "accusative",
    "instrumental",
    "prepositional",
    "locative",
    "ablative",
    "vocative",
  )

  /** Numbers, the columns of a noun's table. */
  private val numbers = List("singular", "dual", "plural")

  /**
    * Tags that only say how rare, formal or regular a form is, or with which
    * article it goes, which a cell does not tell apart.
    */
  private val incidental = Set(
    "formal",
    "informal",
    "rare",
    "archaic",
    "dated",
    "obsolete",
    "nonstandard",
    "colloquial",
    "irregular",
    "vos-form",
    "definite",
    "indefinite",
  )

  /** The tags of the forms that a card names before its tables, in order. */
  private val principals = List(
    "infinitive",
    "past",
    "participle past",
    "participle present",
    "gerund",
    "auxiliary",
    "genitive",
    "plural",
    "comparative",
    "superlative",
  ).map(_.split(' ').toSet)

  /** Tense and mood, in the order that a column's name gives them. */
  private val tenses = List(
    "present",
    "past",
    "preterite",
    "imperfect",
    "perfect",
    "pluperfect",
    "future",
    "conditional",
    "subjunctive",
    "imperative",
  )

  /**
    * The forms of a word that a card names before its tables, as its past and
    * participle, or its genitive and plural, each with what it is.
    */
  def principal(forms: List[Form]): List[(String, String)] = principals.flatMap(
    tags =>
      Option(cell(forms.filter(form => essential(form) == tags)))
        .filter(_.nonEmpty)
        .map(name(tags) -> _),
  )

  /**
    * The tables of the forms of a word of the `pos` part of speech: a verb's by
    * person and number, and by tense and mood, and a noun's or a pronoun's by
    * case and number.
    */
  def tables(pos: String, forms: List[Form]): List[Table] = pos match
    case "verb"                   => conjugation(forms).toList
    case "noun" | "pron" | "name" => declension(forms).toList
    case _                        => Nil

  private def conjugation(forms: List[Form]): Option[Table] =
    val personal = forms.filter(form =>
      !form.tags.contains("multiword-construction") &&
      persons.exists(_.subsetOf(form.tags.toSet)),
    )
    val columns = personal.map(tense).distinct
    Option.when(columns.nonEmpty)(Table(
      "Conjugation",
      columns.map(name),
      persons.map(person =>
        Row(
          name(person),
          columns.map(column =>
            cell(personal.filter(form =>
              person.subsetOf(form.tags.toSet) && tense(form) == column,
            )),
          ),
        ),
      ),
    ))

  /** The tags of a personal form beyond its person and number: its tense. */
  private def tense(form: Form): Set[String] = essential(form) --
    persons.flatten

  private def declension(forms: List[Form]): Option[Table] =
    val rows    = cases.filter(name => forms.exists(_.tags.contains(name)))
    val columns = numbers.filter(number =>
      forms.exists(form =>
        form.tags.contains(number) && rows.exists(form.tags.contains),
      ),
    )
    Option.when(rows.nonEmpty && columns.nonEmpty)(Table(
      "Declension",
      columns.map(_.capitalize),
      rows.map(row =>
        Row(
          row.capitalize,
          columns.map(number =>
            cell(forms.filter(form =>
              form.tags.contains(row) && form.tags.contains(number),
            )),
          ),
        ),
      ),
    ))

  /** The tags of `form` that say what it is, without those incidental to it. */
  private def essential(form: Form): Set[String] = form.tags.toSet -- incidental

  /** The forms of `forms`, each once, without Wiktionary's footnote marks. */
  private def cell(forms: List[Form]): String = forms
    .map(_.form.replaceAll("\\^.*$", "").trim)
    .filter(_.nonEmpty)
    .distinct
    .mkString(", ")

  /**
    * The name of a person, a tense or a principal form, from its `tags`, as
    * `3rd plural` or `Subjunctive II`.
    */
  private def name(tags: Set[String]): String =
    if tags.exists(_.endsWith("-person")) then person(tags)
    else
      val shown = tags --
        (if tags.size > 1 then Set("indicative") else Set.empty) --
        (if tags.exists(_.startsWith("subjunctive-")) then Set("subjunctive")
         else Set.empty)
      shown
        .toList
        .sortBy(tag =>
          tenses.indexOf(tag) match
            case -1    => tenses.size
            case index => index,
        )
        .map {
          case "subjunctive-i"  => "subjunctive I"
          case "subjunctive-ii" => "subjunctive II"
          case other            => other
        }
        .mkString(" ")
        .capitalize

  private def person(tags: Set[String]): String =
    val ordinal = List(
      "first"  -> "1st",
      "second" -> "2nd",
      "third"  -> "3rd",
    ).collectFirst { case (word, short) if tags(s"$word-person") => short }
    s"${ ordinal.getOrElse("") } ${
        if tags("plural") then "plural" else "singular"
      }"
