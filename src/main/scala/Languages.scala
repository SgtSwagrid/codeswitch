package com.alecdorrington.codeswitch

import com.alecdorrington.megalingual.Language
import com.alecdorrington.megalingual.Language.{Chinese, Japanese, Thai}

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
