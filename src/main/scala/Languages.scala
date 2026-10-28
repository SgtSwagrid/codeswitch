package com.alecdorrington.codeswitch

import com.alecdorrington.megalingual.{Language, Localised}
import com.alecdorrington.megalingual.Language.{Chinese, Japanese, Thai}

/**
  * The languages a reader may learn or read in: each a variety where it has
  * them, as a broader language (Chinese, in no script) is too vague to learn or
  * explain in. A broader one, stored before or preferred by a browser (`de`),
  * reads as its first variety.
  */
val learnable: Localised[Language] = Localised.only(
  Language.BritishEnglish,
  Language.values.toList.filter(_.children.isEmpty)*,
)

/**
  * Whether the language's words are written apart, so that they can be found:
  * not in Chinese, Japanese or Thai.
  */
extension (language: Language)

  def spaced: Boolean = !language.within(Chinese) && language != Japanese &&
    language != Thai

/**
  * Whether the language is `other`, whatever the variety of either (British and
  * American English are alike), as a page in either is read alike.
  */
extension (language: Language)

  def alike(other: Language): Boolean = language.lineage.last ==
    other.lineage.last
