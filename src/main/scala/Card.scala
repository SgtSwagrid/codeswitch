package com.alecdorrington.codeswitch

import com.alecdorrington.megalingual.Language
import io.circe.Codec

/**
  * What the card of a word shows: its dictionary `entry`, its `principal`
  * forms, each with what it is, as `Past participle`, the `tables` of its
  * forms, and the `page` of Wiktionary that the entry is from, if it is, which
  * the card credits.
  */
final case class Card
  (
    entry: Entry,
    principal: List[(String, String)],
    tables: List[Table],
    page: Option[String],
  )
  derives Codec

object Card:

  /**
    * The card of `entry`, a word of `language`, its forms laid out by
    * [[Forms]], which only the background does, so that the pages need not.
    */
  def of(language: Language, entry: Entry): Card = Card(
    entry,
    Forms.principal(entry.forms),
    Forms.tables(language, entry.pos, entry.forms),
    entry.source match
      case Source.Wiktionary(_, _) =>
        Some(Wiktionary.page(language, entry.word))
      case Source.Model => None,
  )
