package com.alecdorrington.codeswitch.models

import com.alecdorrington.megalingual.Language
import io.circe.Codec

/**
  * A word or phrase of a passage in a language being learnt, `original`, that
  * is above the reader's level, with its `meaning` in their own language, which
  * may give several translations, the one of them that reads in its place,
  * `gloss`, shown as `kind` says, its dictionary form, `word`, as `anrufen` for
  * `ruft … an`, of `language`, by which the reader rates it, its word class,
  * `wordClass`, as a dictionary names it (`noun`, `verb`, `adj`, ...), if the
  * model gave it, and its CEFR `level`, its difficulty, if known.
  */
final case class Hint
  (
    original: String,
    meaning: String,
    gloss: String,
    kind: Hint.Kind,
    word: String,
    language: Language,
    wordClass: String = "",
    level: Option[Level] = None,
  )
  derives Codec

object Hint:

  /** How a word or phrase above the reader's level is shown. */
  enum Kind derives Codec:

    /**
      * As it is, highlighted, with its meaning on hovering over it, as it is
      * among the words that the reader most needs to learn (see [[Priority]]).
      */
    case Explained

    /**
      * As its gloss, in the reader's own language, with the original on
      * hovering over it, as the reader is not yet familiar with it.
      */
    case Glossed

    /**
      * As it is, only faintly marked, with its meaning on hovering over it, as
      * the reader is familiar with it, and may yet say otherwise.
      */
    case Known

/**
  * Passages of a page in `learnt`, a language being learnt, `texts`, in which
  * to find the words above the reader's level, and whether each is read for the
  * first time in the page's life, `fresh`, so that its words are counted as met
  * only once (see [[services.Counting]]).
  */
final case class Passages
  (
    learnt: Learnt,
    texts: List[String],
    fresh: List[Boolean],
  )
  derives Codec

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
  ):

  /**
    * The word that the card of the hint is of, of the hint's level, met as the
    * page writes it.
    */
  def headword: Headword = Headword(
    hint.word,
    hint.language,
    hint.wordClass,
    hint.meaning,
    tip,
    hint.level,
    hint.original,
  )

/**
  * A word of a language being learnt that the reader met, of which a card is
  * shown: its dictionary form, `word`, of `language`, its word class,
  * `wordClass`, if known, its `meaning` where they met it, in the sense of
  * which it is looked up, `tip`, what the card shows beneath it, its CEFR
  * `level`, its difficulty, if known, from which a word not yet rated is taken
  * to be known as well as it is (see [[Familiarity.default]]), and the `form`
  * in which they met it, as the page shows it, if known, which the card marks
  * among the word's forms.
  */
final case class Headword
  (
    word: String,
    language: Language,
    wordClass: String,
    meaning: String,
    tip: String,
    level: Option[Level],
    form: String = "",
  )
  derives Codec
