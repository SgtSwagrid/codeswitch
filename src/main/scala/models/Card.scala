package com.alecdorrington.codeswitch.models

import com.alecdorrington.megalingual.Language
import io.circe.Codec

/**
  * What the card of a word shows: its dictionary `entry`, the place among its
  * senses of the one `fitting` the meaning in which it was looked up, if any
  * does, the `tables` of its forms, its principal forms first, the `recording`
  * of it that the reader hears, if any, and the `page` of Wiktionary that the
  * entry is from, if it is, which the card credits.
  */
final case class Card
  (
    entry: Entry,
    fitting: Option[Int],
    tables: List[Table],
    recording: Option[Recording],
    page: Option[String],
  )
  derives Codec

object Card:

  /**
    * The card of `entry`, a word of `language`, looked up in the sense of
    * `meaning`, its forms laid out by [[Forms]], its principal forms in a table
    * of their own but those its other tables repeat, the sense that fits found
    * by [[Wiktionary.fitting]], and the recording that a reader of `language`
    * hears chosen by [[Wiktionary.heard]], which only the background does, so
    * that the pages need not.
    */
  def of
    (
      language: Language,
      entry: Entry,
      meaning: String,
    )
    : Card = Card(
    entry,
    Wiktionary.fitting(entry, meaning),
    Forms.principal(language, entry.wordClass, entry.forms).toList ++
      Forms.tables(language, entry.wordClass, entry.forms),
    Wiktionary.heard(language, entry.recordings),
    entry.source match
      case Source.Wiktionary(_, _) =>
        Some(Wiktionary.page(language, entry.word))
      case Source.Model => None,
  )
