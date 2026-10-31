package com.alecdorrington.codeswitch

import io.circe.Codec

/**
  * A dictionary's entry for `word`, a dictionary form of a language, as the
  * `pos` part of speech, of the grammatical `genders` it takes in every sense,
  * if any (several if it may take any of them): its `senses`, its `forms`, each
  * tagged with what it is, how it is said, in `ipa`, where it came from, its
  * `etymology`, and the `source` of the entry. Its shape is Wiktionary's, as
  * Kaikki gives it, so that an entry from there and one from a model read
  * alike.
  */
final case class Entry
  (
    word: String,
    pos: String,
    genders: List[String],
    senses: List[Sense],
    forms: List[Form],
    ipa: Option[String],
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

/** Where an entry came from, which the card credits. */
enum Source derives Codec:

  /**
    * Wiktionary, by way of Kaikki, at `url`, its glosses, its etymology and the
    * translations of its examples put into the reader's language by a model if
    * `translated`.
    */
  case Wiktionary(url: String, translated: Boolean)

  /** A large language model, as it knew the word, when Wiktionary had none. */
  case Model

/**
  * A word to look up: `word`, a dictionary form of the language being learnt,
  * as the `pos` part of speech, if known, in the sense of `meaning`: a hint's,
  * in the reader's own language, or the word of the page that a translation
  * stands for.
  */
final case class Lookup(word: String, pos: String, meaning: String)
  derives Codec

object Lookup:

  /** The parts of speech that a lookup may name, as Kaikki names them. */
  val parts = List(
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
    * `lookup`, trimmed, and with no part of speech if it names none of
    * [[parts]], if it has a word, and neither it nor its meaning is longer than
    * [[longest]], as a page's content script, which asks, may be the page's.
    */
  def checked(lookup: Lookup): Option[Lookup] =
    val (word, meaning) = (lookup.word.trim, lookup.meaning.trim)
    Option.when(
      word.nonEmpty && word.length <= longest && meaning.length <= longest,
    )(Lookup(word, part(lookup.pos), meaning))

  /**
    * `pos`, trimmed and in lower case, if it is one of [[parts]], or else none
    * (`""`).
    */
  def part(pos: String): String = Some(pos.trim.toLowerCase)
    .filter(parts.contains)
    .getOrElse("")
