package com.alecdorrington.codeswitch.models

import com.alecdorrington.megalingual.Language
import com.alecdorrington.omnibrowser.megalingual.Voice
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
    * tagged with, and only the forms that a card shows (see [[Forms.kept]]),
    * and the recordings that a reader hears (see [[audible]]).
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
      word      <- cursor.get[String]("word").toOption
      wordClass <- cursor.get[String]("pos").toOption
    yield Entry(
      word,
      wordClass,
      agreed(senses.map(_._2)),
      senses.map(_._1),
      Forms.kept(language, wordClass, forms(cursor)),
      list(cursor.downField("sounds"))(_.get[String]("ipa").toOption)
        .headOption,
      audible(
        language,
        list(cursor.downField("sounds"))(recording).distinct,
      ),
      cursor.get[String]("etymology_text").toOption.filter(_.nonEmpty),
      Source.Wiktionary(url, translated = false),
    )

  /**
    * The forms of the entry at `cursor` that a table may lay out: those of the
    * word's heading, and of the first table from each source (see
    * [[firstTables]]), but those that only name a table or template.
    */
  private[codeswitch] def forms(cursor: ACursor): List[Form] =
    firstTables(list(cursor.downField("forms"))(form)).filterNot(meta)

  /** What `read` reads of each element of the array at `cursor`, if any. */
  private[codeswitch] def list[A]
    (cursor: ACursor)
    (read: ACursor => Option[A])
    : List[A] = cursor.values.toList.flatten.flatMap(json => read(json.hcursor))

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
  private[codeswitch] def agreed(tags: List[List[String]]): List[String] = tags
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

  private[codeswitch] def strings(cursor: ACursor): List[String] = cursor
    .as[List[String]]
    .getOrElse(Nil)

  /**
    * Where the recordings of Wiktionary's words are, on Wikimedia Commons,
    * which allows requests from any origin.
    */
  private val uploads = "https://upload.wikimedia.org/wikipedia/commons/"

  /** Whether `url` is of a file on Wikimedia Commons (see [[uploads]]). */
  def recorded(url: String): Boolean = url.startsWith(uploads)

  /** The recording of a sound at `cursor`, if it is one, as an MP3. */
  private[codeswitch] def recording(cursor: ACursor): Option[Recording] =
    for
      file <- cursor.get[String]("audio").toOption
      url  <- cursor.get[String]("mp3_url").toOption.filter(recorded)
    yield Recording(
      file,
      url,
      strings(cursor.downField("tags")),
    )

  /**
    * The varieties whose speakers a recording may be told to be of, each with
    * the tags that Wiktionary gives the recording of a speaker from where it is
    * spoken.
    */
  private val accents: List[(Language, Set[String])] = List(
    Language.BritishEnglish -> Set(
      "UK",
      "Received-Pronunciation",
      "England",
      "Southern-England",
    ),
    Language.AmericanEnglish      -> Set("US", "General-American"),
    Language.AustralianEnglish    -> Set("Australia", "General-Australian"),
    Language.GermanGerman         -> Set("Germany"),
    Language.GermanOfSwitzerland  -> Set("Switzerland"),
    Language.BrazilianPortuguese  -> Set("Brazil"),
    Language.EuropeanPortuguese   -> Set("Portugal"),
    Language.EuropeanSpanish      -> Set("Spain"),
    Language.LatinAmericanSpanish -> Set(
      "Latin-America",
      "Mexico",
      "Argentina",
      "Colombia",
      "Chile",
      "Peru",
      "Venezuela",
      "Cuba",
      "US",
    ),
    Language.SimplifiedChinese -> Set("Mandarin"),
    Language.TaiwaneseChinese  -> Set("Mandarin"),
    Language.Cantonese         -> Set("Cantonese"),
  )

  /**
    * Whether Wiktionary's entries of `language` are of several lects, each
    * recording of one, as Chinese's are of Mandarin, Cantonese, Min and others,
    * so that a reader hears only their own.
    */
  private def lectal(language: Language): Boolean =
    language.alike(Language.Chinese)

  /**
    * How far `recording` is from how a reader of `language` says words, if they
    * would hear it at all: `0` if its speaker is from where the variety that
    * `language` is spoken in is (see [[Voice.spoken]]), `1` if Wiktionary does
    * not say where they are from, and `2` if from anywhere else, but never
    * those in a language of several lects (see [[lectal]]).
    */
  private def distance(language: Language, recording: Recording): Option[Int] =
    if from(Voice.spoken(language), recording) then Some(0)
    else Option.when(!lectal(language))(if recording.tags.isEmpty then 1 else 2)

  /**
    * Whether the speaker of `recording` is from where `variety` is spoken, as
    * its tags say (see [[accents]]).
    */
  private def from(variety: Language, recording: Recording): Boolean = accents
    .exists((place, tags) =>
      variety.within(place) && recording.tags.exists(tags),
    )

  /**
    * Of `recordings`, of a word of `language`, the one that a reader of it
    * hears: the first of the [[distance]] nearest, if they hear any.
    */
  def heard
    (
      language: Language,
      recordings: List[Recording],
    )
    : Option[Recording] = recordings
    .flatMap(recording => distance(language, recording).map(_ -> recording))
    .minByOption(_._1)
    .map(_._2)

  /**
    * Of `recordings`, of a word of `language`, those that a reader of any of
    * its varieties hears, in their order, as an entry keeps only those, which
    * are kept by the whole language.
    */
  private[codeswitch] def audible
    (
      language: Language,
      recordings: List[Recording],
    )
    : List[Recording] =
    val kept = (language.whole :: language.whole.descendants)
      .flatMap(heard(_, recordings))
      .toSet
    recordings.filter(kept)

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
    * them: those with senses, and of its word class, if any are, as when a word
    * is two words, as `Bank`, a bench, and `Bank`, a bank.
    */
  def candidates(entries: List[Entry], lookup: Lookup): List[(Entry, Int)] =
    val glossed = entries.zipWithIndex.filter(_._1.senses.nonEmpty)
    val fitting = glossed.filter(_._1.wordClass == lookup.wordClass)
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

  /**
    * The place among the senses of `entry` of the one that best fits `meaning`,
    * which is in the language of its glosses, if it has several and any fits at
    * all, or the first of those that fit as well: as `to win` fits
    * `to win; to be victorious` best of `gewinnen`'s, and `to persuade` fits
    * `to win over; to persuade`.
    */
  def fitting(entry: Entry, meaning: String): Option[Int] =
    val senses = entry.senses.map(_.glosses.flatMap(parts))
    val common = senses.flatten.reduceOption(_ intersect _).getOrElse(Set.empty)
    senses
      .map(closeness(_, parts(meaning), common))
      .zipWithIndex
      .filter((closeness, _) => closeness > 0)
      .maxByOption((closeness, index) => (closeness, -index))
      .map(_._2)
      .filter(_ => senses.size > 1)

  /**
    * The parts of `text`, a gloss or a meaning, each the [[telling]] words
    * between its commas or semicolons, as `to call, to ring` has two, but those
    * in brackets, which only qualify them.
    */
  private def parts(text: String): List[Set[String]] = text
    .replaceAll(bracketed, " ")
    .split("[,;，；、]")
    .toList
    .map(part => telling(wordsOf(part).toSet))
    .filter(_.nonEmpty)

  /**
    * Of the `words` of a part of a gloss or a meaning, those that tell it
    * apart: those of more than two letters, as `a`, `to` or `of` tell little,
    * unless it has no others, as `to go` has none.
    */
  private def telling(words: Set[String]): Set[String] =
    Some(words.filter(_.length > 2)).filter(_.nonEmpty).getOrElse(words)

  /** What a gloss or a meaning has in brackets, of any kind. */
  private val bracketed = """\([^()]*\)|\[[^\[\]]*\]|（[^（）]*）"""

  /**
    * How close the closest of a sense's `parts` is to any of a meaning's,
    * `wanted`, but for the `common` words, which every part of a word's senses
    * has, as `the` may, and so tell none apart.
    */
  private def closeness
    (
      parts: List[Set[String]],
      wanted: List[Set[String]],
      common: Set[String],
    )
    : Double = parts
    .flatMap(part =>
      wanted.map(meaning => dice(part -- common, meaning -- common)),
    )
    .maxOption
    .getOrElse(0.0)

  /**
    * How alike two sets of words are, `first` and `second`: twice as many as
    * each has of words akin to the other's, over how many both have, so `1` if
    * each is the other, and `0` if they have none in common.
    */
  private def dice(first: Set[String], second: Set[String]): Double =
    val shared = first.count(word => second.exists(akin(word, _))) min
      second.count(word => first.exists(akin(word, _)))
    if shared == 0 then 0.0 else 2.0 * shared / (first.size + second.size)

  /**
    * Whether `word` and `other` may be one word in two of its forms: the same,
    * or one beginning the other (see [[begins]]), as `call` and `calls`,
    * `called` or `calling`.
    */
  private def akin(word: String, other: String): Boolean = word == other ||
    begins(word, other) || begins(other, word)

  /**
    * Whether `short`, of at least four letters, begins `long`, which has at
    * most three more.
    */
  private def begins(short: String, long: String): Boolean =
    short.length >= 4 && long.startsWith(short) &&
    long.length - short.length <= 3

  private def wordsOf(text: String): List[String] =
    val lower = text.toLowerCase
    Words.find(lower).map(_.in(lower))
