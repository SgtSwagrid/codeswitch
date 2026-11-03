package com.alecdorrington.codeswitch.lexicon

import com.alecdorrington.codeswitch.models.whole
import com.alecdorrington.megalingual.Language
import io.circe.Codec

/**
  * A spelling of `language` read `count` times, perhaps in part, before it was
  * looked up, first read at `since`, which waits to be looked up, so that it is
  * counted toward the lexemes it is a form of once they are known. If it was
  * read at the start of a sentence, `opening`, its capital may be only for
  * that, as `Die` is `die`. A word that hints named, `hinted`, waits too, so
  * that the spellings they were given for are listed among its forms once it is
  * known (see [[services.Counting.heed]]).
  */
final case class Pending
  (
    language: Language,
    spelling: Spelling,
    count: Double,
    opening: Boolean,
    since: Timestamp,
    hinted: List[Hinted] = Nil,
  )
  derives Codec:

  /** What it is kept by: its whole language's code, then its spelling. */
  def key: String = Pending.key(language, spelling)

  /**
    * It, read `count` times more, and at the start of a sentence if `opening`,
    * and named by the hints of `hinted` too.
    */
  def more
    (
      count: Double,
      opening: Boolean,
      hinted: List[Hinted] = Nil,
    )
    : Pending = copy(
    count = this.count + count,
    opening = this.opening || opening,
    hinted = (this.hinted ++ hinted).distinct,
  )

object Pending:

  /** What the waiting `spelling` of `language` is kept by. */
  def key(language: Language, spelling: Spelling): String =
    s"${ language.whole.code }:${ spelling.text }"

/**
  * That `original`, a spelling read, is a form of a word, a `wordClass`, as a
  * hint of it said, from `source`: the model that wrote the hint, reading it on
  * a page.
  */
final case class Hinted
  (
    original: Spelling,
    wordClass: WordClass,
    source: Source,
  )
  derives Codec
