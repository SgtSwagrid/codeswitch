package com.alecdorrington.codeswitch

import com.alecdorrington.megalingual.Language
import io.circe.{ACursor, Decoder, Json}
import io.circe.parser.parse
import scala.scalajs.js.JSStringOps.enableJSStringOps
import scala.scalajs.js.UnicodeNormalizationForm
import scala.scalajs.js.URIUtils.encodeURIComponent

/**
  * Wiktionary's entries, as Kaikki publishes them: a file of JSON lines for
  * each word of each language, each line an entry, at a path of the word's
  * first letter, then its first two, then the word.
  */
object Wiktionary:

  /** Where Kaikki's dictionaries are. */
  private val site = "https://kaikki.org/dictionary"

  /**
    * The pages that the background fetches Kaikki's entries from, which it
    * needs to be granted, as Kaikki does not allow requests from other origins.
    */
  val host = "https://kaikki.org/*"

  /**
    * The page of English Wiktionary, from which Kaikki takes its entries, for
    * `word`, of `language`, at its section for the language.
    */
  def page(language: Language, word: String): String = List(
    "https://en.wiktionary.org/wiki/",
    encodeURIComponent(word.replace(' ', '_')),
    "#",
    encodeURIComponent(nameOf(language).replace(' ', '_')),
  ).mkString

  /**
    * The name of `language` in Kaikki's dictionaries, which are by name, and of
    * whole languages, not their varieties: its whole language's name in English
    * (see [[whole]]), but for those Wiktionary names otherwise.
    */
  def nameOf(language: Language): String = language.whole match
    case Language.Norwegian                   => "Norwegian Bokmål"
    case Language.Croatian | Language.Serbian => "Serbo-Croatian"
    case Language.Filipino                    => "Tagalog"
    case Language.Slovenian                   => "Slovene"
    case Language.SwissGerman                 => "Alemannic German"
    case whole                                => whole.name(Language.English)

  /**
    * Where Kaikki keeps the entries for `word`, of `language`: under its name
    * as Kaikki writes it, composed (NFC), with `.` as `_dot_` and `/` as
    * `_slash_`, at a path of the first letter of that name, then its first two.
    */
  def url(language: Language, word: String): String =
    val name = word
      .normalize(UnicodeNormalizationForm.NFC)
      .replace(".", "_dot_")
      .replace("/", "_slash_")
    (site :: List(
      nameOf(language),
      "meaning",
      letters(name, 1),
      letters(name, 2),
      s"$name.jsonl",
    ).map(encodeURIComponent)).mkString("/")

  /** The first `count` letters of `word`, whole, though some take two chars. */
  private def letters(word: String, count: Int): String = word.substring(
    0,
    word.offsetByCodePoints(
      0,
      count.min(word.codePointCount(0, word.length)),
    ),
  )

  /**
    * The entries in `lines`, Kaikki's file at `url` of words of `language`,
    * each with the senses that have glosses, the genders that all of those are
    * tagged with, and only the forms that a card shows (see [[Forms.kept]]).
    */
  def entries
    (
      language: Language,
      lines: String,
      url: String,
    )
    : List[Entry] = lines
    .linesIterator
    .filter(_.trim.nonEmpty)
    .flatMap(line => parse(line).toOption.flatMap(entry(_, language, url)))
    .toList

  private def entry
    (
      json: Json,
      language: Language,
      url: String,
    )
    : Option[Entry] =
    val cursor = json.hcursor
    val senses = list(cursor.downField("senses"))(sense)
    for
      word <- cursor.get[String]("word").toOption
      pos  <- cursor.get[String]("pos").toOption
    yield Entry(
      word,
      pos,
      agreed(senses.map(_._2)),
      senses.map(_._1),
      Forms.kept(
        language,
        pos,
        firstTables(list(cursor.downField("forms"))(form)).filterNot(meta),
      ),
      list(cursor.downField("sounds"))(_.get[String]("ipa").toOption)
        .headOption,
      cursor.get[String]("etymology_text").toOption.filter(_.nonEmpty),
      Source.Wiktionary(url, translated = false),
    )

  /** What `read` reads of each element of the array at `cursor`, if any. */
  private def list[A](cursor: ACursor)(read: ACursor => Option[A]): List[A] =
    cursor.values.toList.flatten.flatMap(json => read(json.hcursor))

  /** The sense at `cursor`, with its tags, if it has glosses. */
  private def sense(cursor: ACursor): Option[(Sense, List[String])] =
    Some(Sense(
      strings(cursor.downField("glosses")),
      list(cursor.downField("examples"))(example),
    )).filter(_.glosses.nonEmpty).map(_ -> strings(cursor.downField("tags")))

  /**
    * The genders that each of a word's senses, tagged with `tags`, is tagged
    * with, as Wiktionary tags each sense with the genders of its heading: none
    * if it has no senses.
    */
  private def agreed(tags: List[List[String]]): List[String] = tags
    .map(Entry.gendersOf)
    .reduceOption((first, second) => first.filter(second.contains))
    .getOrElse(Nil)

  private def example(cursor: ACursor): Option[Example] = cursor
    .get[String]("text")
    .toOption
    .map(Example(
      _,
      cursor
        .get[String]("translation")
        .orElse(cursor.get[String]("english"))
        .toOption,
    ))

  private def form(cursor: ACursor): Option[Form] = cursor
    .get[String]("form")
    .toOption
    .map(form =>
      Form(
        form,
        strings(cursor.downField("tags")),
        cursor.get[String]("source").toOption,
      ),
    )

  private def strings(cursor: ACursor): List[String] = cursor
    .as[List[String]]
    .getOrElse(Nil)

  /** Tags of forms that only name the tables and templates they came from. */
  private val metaTags = Set(
    "table-tags",
    "inflection-template",
    "class",
    "romanization",
  )

  private def meta(form: Form): Boolean = form.tags.exists(metaTags)

  /**
    * Of `forms`, those of the word's heading, and of the first table from each
    * source, each table beginning with a form that names it: a verb's own, and
    * not those after it, of its forms with pronouns joined to it, its reflexive
    * or its subordinate clauses, or its forms as spelt before a reform.
    */
  private def firstTables(forms: List[Form]): List[Form] =
    val tables = forms
      .scanLeft(Map.empty[String, Int])((seen, form) =>
        form.source.filter(_ => form.tags.contains("table-tags")) match
          case Some(source) =>
            seen.updated(source, seen.getOrElse(source, 0) + 1)
          case None => seen,
      )
      .tail
    forms
      .zip(tables)
      .collect:
        case (form, seen) if form.source.forall(seen.getOrElse(_, 0) <= 1) =>
          form

  /** The most of a word's entries that may be its own, as it has few. */
  private val most = 4

  /**
    * The entries of `entries` that may be `lookup`'s, each with its place among
    * them: those with senses, and of its part of speech, if any are, as when a
    * word is two words, as `Bank`, a bench, and `Bank`, a bank.
    */
  def candidates(entries: List[Entry], lookup: Lookup): List[(Entry, Int)] =
    val glossed = entries.zipWithIndex.filter(_._1.senses.nonEmpty)
    val fitting = glossed.filter(_._1.pos == lookup.pos)
    (if fitting.isEmpty then glossed else fitting).take(most)

  /**
    * The entry of `entries` with a gloss that best fits `meaning`, which is in
    * the language of the glosses, or the first of those that fit as well.
    */
  def choose(entries: List[Entry], meaning: String): Option[Entry] = entries
    .zipWithIndex
    .maxByOption((entry, index) => (overlap(entry, meaning), -index))
    .map(_._1)

  /**
    * How well a gloss of `entry` fits `meaning`: the greatest share of the
    * words of any one gloss that are words of `meaning`, so that a short gloss
    * that is all but the meaning fits better than a long one that holds it.
    */
  private def overlap(entry: Entry, meaning: String): Double =
    val wanted = wordsOf(meaning).toSet
    entry
      .senses
      .flatMap(_.glosses)
      .map(wordsOf)
      .filter(_.nonEmpty)
      .map(gloss => gloss.count(wanted).toDouble / gloss.size)
      .maxOption
      .getOrElse(0.0)

  private def wordsOf(text: String): List[String] =
    val lower = text.toLowerCase
    Words.find(lower).map(_.in(lower))
