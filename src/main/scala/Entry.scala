package com.alecdorrington.codeswitch

import io.circe.{Codec, Decoder, Encoder}

/**
  * A dictionary's entry for `word`, a dictionary form of a language, as the
  * `pos` part of speech: its `senses`, its `forms`, each tagged with what it
  * is, how it is said, in `ipa`, where it came from, its `etymology`, and the
  * `source` of the entry. Its shape is Wiktionary's, as Kaikki gives it, so
  * that an entry from there and one from a model read alike.
  */
final case class Entry
  (
    word: String,
    pos: String,
    senses: List[Sense],
    forms: List[Form],
    ipa: Option[String],
    etymology: Option[String],
    source: Source,
  )
  derives Codec

/**
  * A sense of a word, as its `glosses` give it, in the reader's own language,
  * with `examples` of its use.
  */
final case class Sense
  (
    glosses: List[String],
    examples: List[Example],
  )

object Sense:

  /** Reads a sense without glosses or examples as having none, as a model may. */
  given Codec[Sense] = Codec.from(
    Decoder.instance(cursor =>
      for
        glosses  <- cursor.getOrElse[List[String]]("glosses")(Nil)
        examples <- cursor.getOrElse[List[Example]]("examples")(Nil)
      yield Sense(glosses, examples),
    ),
    Encoder.AsObject.derived,
  )

/**
  * An example of a word's use, `text`, in its own language, with its
  * `translation` into the reader's, if it has one.
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
  * as the `pos` part of speech, if known, in the sense of `meaning`, in the
  * reader's own language, as a hint gave it.
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
    val pos             = lookup.pos.trim.toLowerCase
    Option.when(
      word.nonEmpty && word.length <= longest && meaning.length <= longest,
    )(Lookup(
      word,
      if parts.contains(pos) then pos else "",
      meaning,
    ))
