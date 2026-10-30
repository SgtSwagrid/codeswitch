package com.alecdorrington.codeswitch

import io.circe.Codec

/**
  * A word or phrase of a passage in the language being learnt, `original`, that
  * is above the reader's level, with its `meaning` in their own language, which
  * may give several translations, the one of them that reads in its place,
  * `gloss`, shown as `kind` says, its dictionary form, `word`, as `anrufen` for
  * `ruft … an`, by which the reader rates it, and its part of speech, `pos`, as
  * a dictionary names it (`noun`, `verb`, `adj`, ...), if the model gave it.
  */
final case class Hint
  (
    original: String,
    meaning: String,
    gloss: String,
    kind: Hint.Kind,
    word: String,
    pos: String = "",
  )
  derives Codec

object Hint:

  /** How a word or phrase above the reader's level is shown. */
  enum Kind derives Codec:

    /**
      * As it is, highlighted, with its meaning on hovering over it, as it is a
      * level above the reader's, and theirs to learn.
      */
    case Explained

    /**
      * As its gloss, in the reader's own language, with the original on
      * hovering over it, as it is further above the reader's level.
      */
    case Glossed

    /**
      * As it is, only faintly marked, with its meaning on hovering over it, as
      * the reader has said that they know it, and may yet say otherwise.
      */
    case Known

/**
  * How a word or phrase of a page that `hint` marks is shown: as `kind` says,
  * as `gloss` in its place, if anything, with `tip` on hovering over it.
  */
final case class Marking
  (
    gloss: Option[String],
    tip: String,
    kind: Hint.Kind,
    hint: Hint,
  )
