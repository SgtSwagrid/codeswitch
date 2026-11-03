package com.alecdorrington.codeswitch.lexicon

import com.alecdorrington.codeswitch.models.whole
import com.alecdorrington.megalingual.Language
import io.circe.Codec

/**
  * A spelling of `language` read `count` times, perhaps in part, before it was
  * looked up, first read at `since`, which waits to be looked up, so that it is
  * counted toward the lexemes it is a form of once they are known. If it was
  * read at the start of a sentence, `opening`, its capital may be only for
  * that, as `Die` is `die`.
  */
final case class Pending
  (
    language: Language,
    spelling: Spelling,
    count: Double,
    opening: Boolean,
    since: Timestamp,
  )
  derives Codec:

  /** What it is kept by: its whole language's code, then its spelling. */
  def key: String = Pending.key(language, spelling)

  /** It, read `count` times more, and at the start of a sentence if `opening`. */
  def more(count: Double, opening: Boolean): Pending = copy(
    count = this.count + count,
    opening = this.opening || opening,
  )

object Pending:

  /** What the waiting `spelling` of `language` is kept by. */
  def key(language: Language, spelling: Spelling): String =
    s"${ language.whole.code }:${ spelling.text }"
