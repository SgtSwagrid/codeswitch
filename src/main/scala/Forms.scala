package com.alecdorrington.codeswitch

import com.alecdorrington.megalingual.Language
import io.circe.Codec

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
  derives Codec

/** A row of a [[Table]], called `label`, with its `cells`. */
final case class Row(label: String, cells: List[String]) derives Codec

/**
  * Lays a word's forms out for the card, by the tags that say what each is, in
  * the same way for every language, as Wiktionary tags them alike. The tables
  * take only the forms of Wiktionary's own tables, and name their columns only
  * by the tenses and moods that they know.
  */
object Forms:

  /** Persons, genders and numbers, the rows of a verb's table, in order. */
  private val persons = List(
    "first-person",
    "second-person",
    "third-person",
  )

  private val genders = List("masculine", "feminine", "neuter")

  private val numbers = List("singular", "dual", "plural")

  /** Tenses, in the order that a column's name gives them, after negative. */
  private val tenses = List(
    "negative",
    "present",
    "past",
    "preterite",
    "aorist",
    "imperfect",
    "perfect",
    "pluperfect",
    "future",
  )

  /** Moods, after the tenses in a column's name. */
  private val moods = List(
    "indicative",
    "conditional",
    "subjunctive",
    "subjunctive-i",
    "subjunctive-ii",
    "imperative",
    "optative",
    "potential",
    "jussive",
  )

  /** Cases, the rows of a noun's table, in their order. */
  private val cases = List(
    "nominative",
    "genitive",
    "partitive",
    "dative",
    "accusative",
    "instrumental",
    "prepositional",
    "locative",
    "inessive",
    "elative",
    "illative",
    "adessive",
    "ablative",
    "allative",
    "essive",
    "translative",
    "instructive",
    "abessive",
    "comitative",
    "vocative",
  )

  /**
    * Tags of forms that are not the word alone: with a pronoun joined to it, or
    * another's, possessed by someone, or of several words.
    */
  private val unalone = Set(
    "combined-form",
    "multiword-construction",
    "possessive",
    "singular-possessive",
    "plural-possessive",
    "possessed-single",
    "possessed-many",
  )

  /**
    * Tags that only say how rare, formal or regular a form is, or with which
    * article it goes, which a principal form does not tell apart.
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

  /** The forms that a card names before its tables, in order. */
  private val principals = List(
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

  /**
    * The forms of a word that a card names before its tables, as its past and
    * participle, or its genitive and plural, each with what it is.
    */
  def principal(forms: List[Form]): List[(String, String)] = principals.flatMap(
    tags =>
      Option(cell(forms.filter(isPrincipal(tags))))
        .filter(_.nonEmpty)
        .map(nameOf(tags) -> _),
  )

  private def isPrincipal(tags: Set[String])(form: Form): Boolean =
    written(form) && form.tags.toSet -- incidental == tags

  /**
    * The languages, by part of speech, whose tables these are checked to lay
    * out as Wiktionary does, against Kaikki's files of them (the tests').
    * Others lay their forms out in ways these tables would not tell apart, as
    * Dutch with no person in the plural, Hungarian with a definite conjugation
    * beside the indefinite, or Polish with genders beside persons, so a word of
    * one has its principal forms alone.
    */
  private val checked: Map[String, Set[Language]] = Map(
    "verb" -> Set(
      Language.German,
      Language.Spanish,
      Language.Italian,
      Language.Portuguese,
      Language.French,
      Language.Finnish,
      Language.Russian,
    ),
    "noun" -> Set(
      Language.German,
      Language.Finnish,
      Language.Russian,
      Language.Turkish,
      Language.Polish,
    ),
  )

  /**
    * The tables of the forms of a word of `language`, as the `pos` part of
    * speech, if its tables are [[checked]]: a verb's by person and number, and
    * by tense and mood, and a noun's by case and number.
    */
  def tables
    (
      language: Language,
      pos: String,
      forms: List[Form],
    )
    : List[Table] =
    val laid = laidOut(language, pos, forms)
    (if pos == "verb" then conjugation(laid) else declension(laid)).toList

  /**
    * Of the forms of a word of `language`, as the `pos` part of speech, those
    * that a card shows, which are all that need be kept of it.
    */
  def kept
    (
      language: Language,
      pos: String,
      forms: List[Form],
    )
    : List[Form] =
    val laid = laidOut(language, pos, forms).toSet
    forms.filter(form => laid(form) || principals.exists(isPrincipal(_)(form)))

  /**
    * The forms that the tables of a word of `language`, as the `pos` part of
    * speech, lay out, if they are [[checked]]: a verb's by person or gender,
    * and number, if they have a tense or mood known, and a noun's by case and
    * number.
    *
    * If any of a verb's columns has forms of a word alone, those of only
    * several words, an auxiliary and the verb, are left out, as Finnish has for
    * each tense, but not those of a separable verb, all of which are of several
    * words in a main clause, as German `rufe an`. So too a noun's of several
    * words, as with an article, unless all are, as an idiom's.
    */
  private def laidOut
    (
      language: Language,
      pos: String,
      forms: List[Form],
    )
    : List[Form] =
    val table = forms.filter(tabular)
    pos match
      case _ if !checked.get(pos).exists(_(language.lineage.last)) => Nil
      case "verb"                                                  =>
        val personal = table.filter(form =>
          !form.tags.exists(nonfinite) && rowOf(form).nonEmpty && tense(form)
            .nonEmpty,
        )
        val alone = personal
          .filterNot(_.form.contains(' '))
          .flatMap(tense)
          .toSet
        if alone.isEmpty then personal
        else personal.filter(tense(_).exists(alone))
      case _ =>
        val declined = table.filter(form =>
          !form.tags.exists(persons.contains) &&
          form.tags.exists(cases.contains) && form.tags.exists(numbers.contains),
        )
        val alone = declined.filterNot(_.form.contains(' '))
        if alone.isEmpty then declined else alone

  /** Whether `form` is from a table of the word's, and the word alone. */
  private def tabular(form: Form): Boolean = form.source.nonEmpty &&
    written(form) &&
    !form.tags.exists(tag => unalone(tag) || tag.startsWith("object-"))

  /**
    * Whether `form` is spelt, not a dash that marks there being no such form,
    * nor a pronunciation, as Wiktionary's French tables give.
    */
  private def written(form: Form): Boolean =
    !Set("-", "–", "—")(form.form.trim) &&
    !form.form.exists(char => char >= 'ɐ' && char <= 'ʯ')

  /** Tags of the forms of a verb that are not of a person, nor a gender. */
  private val nonfinite = Set(
    "infinitive",
    "participle",
    "gerund",
    "supine",
  )

  /**
    * Every tag that the tables and the principal forms are laid out by, with
    * which a model that makes an entry is asked to tag each form.
    */
  val tags: List[String] =
    (cases ++ numbers ++ persons ++ genders ++ tenses ++ moods ++ nonfinite ++
      principals.flatten).distinct

  /**
    * A verb's table, by number, then person, or gender, as a Russian past is,
    * the singular first, as dictionaries lay them out, and by tense and mood. A
    * column is called present only beside another of its mood, or in the
    * indicative, as Finnish has a conditional of one tense.
    */
  private def conjugation(forms: List[Form]): Option[Table] =
    val columns = forms.flatMap(tense).distinct
    val rows    =
      for
        row <- numbers.flatMap(number => (persons ++ genders).map(_ -> number))
        cells = columns.map(column =>
          cell(forms.filter(form =>
            rowOf(form) == Some(row) && tense(form) == Some(column),
          )),
        ) if cells.exists(_.nonEmpty)
      yield Row(rowName(row), cells)
    val names = columns.map(column =>
      val mood  = column -- tenses
      val alone = !mood("indicative") && columns.count(_ -- tenses == mood) == 1
      nameOf(if alone then column - "present" else column),
    )
    Option.when(rows.nonEmpty)(Table("Conjugation", names, rows))

  /**
    * The person, or else the gender, and the number of `form`, if it has one of
    * each.
    */
  private def rowOf(form: Form): Option[(String, String)] = (
    persons.filter(form.tags.contains),
    genders.filter(form.tags.contains),
    numbers.filter(form.tags.contains),
  ) match
    case (List(person), _, List(number))   => Some(person -> number)
    case (Nil, List(gender), List(number)) => Some(gender -> number)
    case _                                 => None

  /**
    * The tense and mood of `form`, of those known, a tense without a mood being
    * indicative, if it has any.
    */
  private def tense(form: Form): Option[Set[String]] =
    val tags  = form.tags.toSet
    val known = tags.filter(tag => tenses.contains(tag) || moods.contains(tag))
    Option
      .when(known.nonEmpty)(known)
      .map(known =>
        if known.exists(moods.contains) then known else known + "indicative",
      )

  private def declension(forms: List[Form]): Option[Table] =
    val rows    = cases.filter(name => forms.exists(_.tags.contains(name)))
    val columns =
      numbers.filter(number => forms.exists(_.tags.contains(number)))
    Option.when(rows.nonEmpty)(Table(
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

  /** The forms of `forms`, each once, without Wiktionary's footnote marks. */
  private def cell(forms: List[Form]): String = forms
    .map(_.form.replaceAll("\\^.*$", "").trim)
    .filter(_.nonEmpty)
    .distinct
    .mkString(", ")

  private def rowName(row: (String, String)): String =
    val (of, number) = row
    val name         = of match
      case "first-person"  => "1st"
      case "second-person" => "2nd"
      case "third-person"  => "3rd"
      case gender          => gender.capitalize
    s"$name $number"

  /**
    * The name of a tense and mood, or of a principal form, from its known
    * `tags`, in a fixed order, as `Imperfect subjunctive` or `Past participle`.
    * Indicative is named only alone, and subjunctive not beside its kind.
    */
  private def nameOf(tags: Set[String]): String =
    val shown = tags.filterNot(tag =>
      (tag == "indicative" && tags.size > 1) ||
      (tag == "subjunctive" && tags.exists(_.startsWith("subjunctive-"))),
    )
    val order = tenses ++ moods
    shown
      .toList
      .sortBy(tag =>
        (
          order.indexOf(tag) match
            case -1    => order.size
            case index => index,
          tag,
        ),
      )
      .map {
        case "subjunctive-i"  => "subjunctive I"
        case "subjunctive-ii" => "subjunctive II"
        case other            => other
      }
      .mkString(" ")
      .capitalize
