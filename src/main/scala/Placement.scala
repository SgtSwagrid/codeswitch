package com.alecdorrington.codeswitch

import cats.syntax.all.*
import com.alecdorrington.omnibrowser.Codec

/** Where a piece of a word's translation is shown. */
enum Place derives Codec:

  /** In place of the word. */
  case Home

  /** In place of a `run` of words that includes the word, as `the apple`. */
  case Replacing(run: String)

  /** Just after the nearest `anchor`, a word or words of the word's sentence. */
  case After(anchor: String)

  /** Just before the nearest `anchor`. */
  case Before(anchor: String)

/** A piece of a word's translation, `text`, shown at `place`. */
final case class Piece(text: String, place: Place) derives Codec

/**
  * The translation of a word, in `pieces`, each shown in its own place, as the
  * language's word order needs. With none, the word is left as it is.
  */
final case class Translation(pieces: List[Piece]) derives Codec

object Translation:

  /** The word, left as it is. */
  val declined: Translation = Translation(Nil)

  /** `text`, in place of the word. */
  def apply(text: String): Translation =
    Translation(List(Piece(text, Place.Home)))

/**
  * A translation laid out in the text of a passage: `shown` in place of its
  * `home`, the word or the run of words it stands for, or nothing, in which
  * case the `space` beside it goes too, so that no two spaces are left
  * together; and the `inserted` pieces elsewhere.
  */
final case class Layout
  (
    home: Span,
    shown: String,
    space: Option[Span],
    inserted: List[Insert],
  )

/**
  * A `piece` of a translation, shown `at` a place in the text, after the text
  * before it if `after`, or else before the text after it.
  */
final case class Insert(at: Int, piece: String, after: Boolean)

/** Finds where to show each piece of a word's translation. */
object Placement:

  /** The most words that a piece may stand in place of. */
  val reach = 3

  /**
    * Where to show `translation` of the word at `word` in `text`, the text of
    * its passage, of which `node` is the word's own text node. It is nowhere if
    * it was declined, or if any of its pieces has no place: a run of words must
    * be within `node`, and hold no digits, and an anchor must be within the
    * word's sentence, which is only found if a piece goes beside one.
    */
  def lay
    (
      text: String,
      node: Span,
      word: Span,
      translation: Translation,
    )
    : Option[Layout] =
    val shown = translation
      .pieces
      .collect { case Piece(piece, Place.Home | Place.Replacing(_)) => piece }
      .mkString(" ")
    lazy val sentence = Words.sentenceAt(text, word.start)
    for
      _        <- translation.pieces.headOption
      home     <- homeOf(text, node, word, translation)
      inserted <- translation
        .pieces
        .flatTraverse(insert(text, sentence, home, _))
    yield Layout(
      home,
      Words.matchCase(home.in(text), shown),
      Option.when(shown.isEmpty)(spaceBeside(text, home)).flatten,
      inserted,
    )

  /**
    * Where the pieces in place of the word go: the word itself, or the run of
    * words that they replace, so long as they agree on one.
    */
  private def homeOf
    (
      text: String,
      node: Span,
      word: Span,
      translation: Translation,
    )
    : Option[Span] = translation
    .pieces
    .collect { case Piece(_, Place.Replacing(run)) => run }
    .distinct match
      case Nil        => Some(word)
      case run :: Nil => runOf(text, node, word, run)
      case _          => None

  /**
    * Where `run` is, around `word` and within `node`, unless it is longer than
    * [[reach]] or holds a number, which the page's own text should keep.
    */
  private def runOf
    (
      text: String,
      node: Span,
      word: Span,
      run: String,
    )
    : Option[Span] = Words
    .occurrences(text, run)
    .find(span => span.covers(word) && node.covers(span))
    .filter(_ =>
      run.trim.split("\\s+").length <= reach && !run.exists(_.isDigit),
    )

  /** Where `piece` goes, if it goes beside an anchor, in `sentence`. */
  private def insert
    (
      text: String,
      sentence: => Span,
      home: Span,
      piece: Piece,
    )
    : Option[List[Insert]] = piece.place match
    case Place.After(anchor) => nearest(text, sentence, home, anchor).map(
        span => List(Insert(span.end, piece.text, true)),
      )
    case Place.Before(anchor) => nearest(text, sentence, home, anchor).map(
        span => List(Insert(span.start, piece.text, false)),
      )
    case _ => Some(Nil)

  /** The occurrence of `anchor` in `sentence` nearest to `home`, beside it. */
  private def nearest
    (
      text: String,
      sentence: Span,
      home: Span,
      anchor: String,
    )
    : Option[Span] = Words
    .occurrences(text, anchor)
    .filter(span => sentence.covers(span) && !span.overlaps(home))
    .minByOption(_.distance(home))

  /**
    * The space just after `home` in `text`, or else just before it, if any, but
    * never a line break, which may be kept as one in the page.
    */
  private def spaceBeside(text: String, home: Span): Option[Span] =
    List(home.end, home.start - 1)
      .find(at => text.indices.contains(at) && " \t\u00a0".contains(text(at)))
      .map(at => Span(at, at + 1))
