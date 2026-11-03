package com.alecdorrington.codeswitch.models

import com.alecdorrington.megalingual.Language
import io.circe.Codec

/**
  * What the card of a word shows: its dictionary `entry`, the place among its
  * senses of the one `fitting` the meaning in which it was looked up, if any
  * does, the `tables` of its forms, its principal forms first, the `recording`
  * of it that the reader hears, if any, the `page` of Wiktionary that the entry
  * is from, if it is, which the card credits, and the `lexeme` of the reader's
  * lexicon that it is of, by its own, which they may edit.
  */
final case class Card
  (
    entry: Entry,
    fitting: Option[Int],
    tables: List[Table],
    recording: Option[Recording],
    page: Option[String],
    lexeme: Option[String] = None,
  )
  derives Codec

/**
  * What opens the editor of the lexeme of the reader's lexicon whose own is
  * `lexeme`, from the card of its word, met in the sense of `meaning`, on the
  * page at `url`, titled `title`, if it was met on a page.
  */
final case class Opening
  (
    lexeme: String,
    meaning: String,
    url: Option[String],
    title: Option[String],
  )
  derives Codec

object Opening:

  /** The longest own of a lexeme, title or address that opens the editor. */
  val longest = 2000

  /**
    * `opening`, its meaning trimmed and cut to [[Lookup.longest]], without a
    * page's address but of the web, nor a blank title, if its lexeme is not
    * blank, and it is not too long, as a page's content script may send it.
    */
  def checked(opening: Opening): Option[Opening] = Option.when(
    opening.lexeme.nonEmpty &&
    (opening.lexeme :: opening.url.toList ++ opening.title.toList).forall(
      _.length <= longest,
    ),
  )(Opening(
    opening.lexeme,
    opening.meaning.trim.take(Lookup.longest),
    opening
      .url
      .filter(url => url.startsWith("https://") || url.startsWith("http://")),
    opening.title.map(_.trim).filter(_.nonEmpty),
  ))

object Card:

  /**
    * The card of `entry`, a word of `language`, of the lexeme whose own is
    * `lexeme`, looked up in the sense of `meaning`, its forms laid out by
    * [[Forms]], its principal forms in a table of their own but those its other
    * tables repeat, the sense that fits found by [[Wiktionary.fitting]], and
    * the recording that a reader of `language` hears chosen by
    * [[Wiktionary.heard]], which only the background does, so that the pages
    * need not.
    */
  def of
    (
      language: Language,
      entry: Entry,
      meaning: String,
      lexeme: Option[String] = None,
    )
    : Card = Card(
    entry,
    Wiktionary.fitting(entry, meaning),
    Forms.principal(language, entry.wordClass, entry.forms).toList ++
      Forms.tables(language, entry.wordClass, entry.forms),
    Wiktionary.heard(language, entry.recordings),
    entry.source match
      case Source.Wiktionary(_, _, _) =>
        Some(Wiktionary.page(language, entry.word))
      case Source.Model => None,
    lexeme,
  )
