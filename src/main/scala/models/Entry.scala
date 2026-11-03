package com.alecdorrington.codeswitch.models

import com.alecdorrington.megalingual.Language
import io.circe.Codec
import scala.scalajs.js.URIUtils.encodeURIComponent

/**
  * A dictionary's entry for `word`, a dictionary form of a language, a
  * `wordClass`, of the grammatical `genders` it takes in every sense, if any
  * (several if it may take any of them): its `senses`, its `forms`, each tagged
  * with what it is, how it is said, in `ipa`, and by speakers, in `recordings`,
  * where it came from, its `etymology`, and the `source` of the entry. Its
  * shape is Wiktionary's, as Kaikki gives it, so that an entry from there and
  * one from a model read alike.
  */
final case class Entry
  (
    word: String,
    wordClass: String,
    genders: List[String],
    senses: List[Sense],
    forms: List[Form],
    ipa: Option[String],
    recordings: List[Recording],
    etymology: Option[String],
    source: Source,
  )
  derives Codec

object Entry:

  /**
    * The grammatical genders that an entry may give its word, as Wiktionary
    * tags them, in the order that a card names them.
    */
  val genders = List(
    "masculine",
    "feminine",
    "neuter",
    "common",
  )

  /** The genders of [[genders]] that `tags` name, in its order, each once. */
  def gendersOf(tags: List[String]): List[String] =
    genders.filter(tags.contains)

/**
  * A sense of a word.
  *
  * @param glosses
  *   The meanings of the word in this sense, in the reader's own language.
  *
  * @param examples
  *   The examples of the word's use in this sense.
  */
final case class Sense
  (
    glosses: List[String],
    examples: List[Example],
  )
  derives Codec

/**
  * An example of a word's use.
  *
  * @param text
  *   The example, in the word's own language.
  *
  * @param translation
  *   The translation of the example into the reader's own language, if it has
  *   one.
  */
final case class Example
  (
    text: String,
    translation: Option[String],
  )
  derives Codec

/**
  * A form of a word, `form`, which `tags` say what it is, as `dative plural`,
  * from the `source` table it was laid out in, if any, which a form of the word
  * that a dictionary names in its heading has none of.
  */
final case class Form
  (
    form: String,
    tags: List[String],
    source: Option[String] = None,
  )
  derives Codec

/**
  * A recording of a word said by a speaker, from Wikimedia Commons: its `file`
  * there, the `url` of it as an MP3, which every browser plays, and the `tags`
  * that Wiktionary gives it, which say where its speaker is from, as `Germany`
  * or `Berlin`.
  */
final case class Recording
  (
    file: String,
    url: String,
    tags: List[String],
  )
  derives Codec:

  /**
    * The recording's page on Wikimedia Commons, which names its speaker and its
    * licence, and which the card credits.
    */
  def page: String = "https://commons.wikimedia.org/wiki/File:" +
    encodeURIComponent(file.replace(' ', '_'))

/** Where an entry came from, which the card credits. */
enum Source derives Codec:

  /**
    * Wiktionary, by way of Kaikki, at `url`, its glosses, its etymology and the
    * translations of its examples put into the reader's language by a model if
    * `translated`, and some of them rewritten by the reader or a model if
    * `changed`.
    */
  case Wiktionary
    (
      url: String,
      translated: Boolean,
      changed: Boolean = false,
    )

  /** A large language model, as it knew the word, when Wiktionary had none. */
  case Model

/**
  * A word to look up: `word`, a dictionary form of `language`, one being
  * learnt, a `wordClass`, if known, in the sense of `meaning`: a hint's, in the
  * reader's own language, or the word of the page that a translation stands
  * for.
  */
final case class Lookup
  (
    word: String,
    wordClass: String,
    meaning: String,
    language: Language,
  )
  derives Codec

object Lookup:

  /** The word classes that a lookup may name, as Kaikki names them. */
  val wordClasses = List(
    "noun",
    "verb",
    "adj",
    "adv",
    "prep",
    "pron",
    "det",
    "conj",
    "num",
    "intj",
    "particle",
    "phrase",
  )

  /** The longest word or phrase, and meaning, that may be looked up. */
  val longest = 100

  /**
    * `lookup`, trimmed, and with no word class if it names none of
    * [[wordClasses]], if it has a word, and neither it nor its meaning is
    * longer than [[longest]], as a page's content script, which asks, may be
    * the page's.
    */
  def checked(lookup: Lookup): Option[Lookup] =
    val (word, meaning) = (lookup.word.trim, lookup.meaning.trim)
    Option.when(
      word.nonEmpty && word.length <= longest && meaning.length <= longest,
    )(Lookup(
      word,
      wordClass(lookup.wordClass),
      meaning,
      lookup.language,
    ))

  /**
    * The word class that `name` names, trimmed and in lower case, if it is one
    * of [[wordClasses]], or else none (`""`).
    */
  def wordClass(name: String): String = Some(name.trim.toLowerCase)
    .filter(wordClasses.contains)
    .getOrElse("")
