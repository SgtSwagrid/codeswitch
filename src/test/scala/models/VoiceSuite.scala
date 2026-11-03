package com.alecdorrington.codeswitch.models

import com.alecdorrington.megalingual.Language
import com.alecdorrington.megalingual.web.Voice
import munit.FunSuite

class VoiceSuite extends FunSuite:

  /** The tag of the voice, of those of `tags`, that says words of `language`. */
  private def best(language: Language, tags: String*): Option[String] =
    voiceFor(
      language,
      tags.toList.map(tag => Voice(tag, tag, tag, true)),
    ).map(_.tag)

  test(
    "A word is said in a voice of a language alike its own, the nearest in Megalingual's tree, so Swiss German in none of German, nor German in one of Swiss German.",
  ):
    assertEquals(
      best(Language.German, "de-CH", "de-DE"),
      Some("de-DE"),
    )
    assertEquals(
      best(
        Language.SwissStandardGerman,
        "de-DE",
        "de-CH",
      ),
      Some("de-CH"),
    )
    assertEquals(
      best(Language.SwissGerman, "de-CH", "de-DE"),
      None,
    )
    assertEquals(
      best(Language.SwissGerman, "de-CH", "gsw-CH"),
      Some("gsw-CH"),
    )
    assertEquals(best(Language.German, "gsw"), None)
    assertEquals(best(Language.French, "en-US"), None)
