package com.alecdorrington.codeswitch

import io.circe.{ACursor, Decoder, Json}
import io.circe.parser.parse
import scala.scalajs.js.URIUtils.encodeURIComponent

/**
  * Wiktionary's entries, as Kaikki publishes them: a file of JSON lines for
  * each word of each language, each line an entry, at a path of the word's
  * first letter, then its first two, then the word.
  */
object Wiktionary:

  /** Where Kaikki's dictionaries are. */
  private val site = "https://kaikki.org/dictionary"

  /** The name of `language` in Kaikki's dictionaries, which are by name. */
  def nameOf(language: Language): String = language match
    case Language.Norwegian => "Norwegian Bokmål"
    case other              => other.name

  /** Where Kaikki keeps the entries for `word`, of `language`. */
  def url(language: Language, word: String): String =
    (site :: List(
      nameOf(language),
      "meaning",
      letters(word, 1),
      letters(word, 2),
      s"$word.jsonl",
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
    * The entries in `lines`, Kaikki's file at `url`, each with the senses that
    * have glosses, and the forms that are words, leaving out those that only
    * name the tables and templates they came from.
    */
  def entries(lines: String, url: String): List[Entry] = lines
    .linesIterator
    .filter(_.trim.nonEmpty)
    .flatMap(line => parse(line).toOption.flatMap(entry(_, url)))
    .toList

  private def entry(json: Json, url: String): Option[Entry] =
    val cursor = json.hcursor
    for
      word <- cursor.get[String]("word").toOption
      pos  <- cursor.get[String]("pos").toOption
    yield Entry(
      word,
      pos,
      list(cursor.downField("senses"))(sense).filter(_.glosses.nonEmpty),
      list(cursor.downField("forms"))(form).filterNot(meta),
      list(cursor.downField("sounds"))(_.get[String]("ipa").toOption)
        .headOption,
      cursor.get[String]("etymology_text").toOption.filter(_.nonEmpty),
      Source.Wiktionary(url, translated = false),
    )

  /** What `read` reads of each element of the array at `cursor`, if any. */
  private def list[A](cursor: ACursor)(read: ACursor => Option[A]): List[A] =
    cursor.values.toList.flatten.flatMap(json => read(json.hcursor))

  private def sense(cursor: ACursor): Option[Sense] = Some(Sense(
    strings(cursor.downField("glosses")),
    list(cursor.downField("examples"))(example),
  ))

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
    * The entry of `entries` for `lookup`: one of its part of speech, if any is,
    * and of those, the one with a gloss that best fits its meaning, as when a
    * word is two words, as `Bank`, a bench, and `Bank`, a bank.
    */
  def choose(entries: List[Entry], lookup: Lookup): Option[Entry] =
    val fitting = entries.filter(_.pos == lookup.pos)
    (if fitting.isEmpty then entries else fitting)
      .zipWithIndex
      .maxByOption((entry, index) => (overlap(entry, lookup.meaning), -index))
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
