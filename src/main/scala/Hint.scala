package com.alecdorrington.codeswitch

import io.circe.Codec

/**
  * A word or phrase of a passage in the language being learnt, `original`, that
  * is above the reader's level, with its `meaning` in their own language, shown
  * as `kind` says, and its dictionary form, `word`, as `anrufen` for `ruft …
  * an`, by which the reader rates it.
  */
final case class Hint
  (
    original: String,
    meaning: String,
    kind: Hint.Kind,
    word: String,
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
      * As its meaning, in the reader's own language, with the original on
      * hovering over it, as it is further above the reader's level.
      */
    case Glossed

    /**
      * As it is, only faintly marked, with its meaning on hovering over it, as
      * the reader has said that they know it, and may yet say otherwise.
      */
    case Known

/**
  * How a word or phrase of a page that `hint` marks is shown: as `shown`, as
  * `kind` says, with `tip` on hovering over it.
  */
final case class Look
  (
    shown: String,
    tip: String,
    kind: Hint.Kind,
    hint: Hint,
  )
