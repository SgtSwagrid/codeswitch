package com.alecdorrington.codeswitch.models

import com.alecdorrington.megalingual.Language
import munit.FunSuite

class VoiceSuite extends FunSuite:

  /**
    * The tag of the voice, of those of `tags`, each on the device unless marked
    * `~` for online, that best says words of `language`, if any.
    */
  private def best(language: Language, tags: String*): Option[String] = Voice
    .best(language, tags.toList)(tag =>
      Voice(
        tag.stripPrefix("~"),
        !tag.startsWith("~"),
      ),
    )

  test(
    "A word is said in the voice of its own variety, or else the nearest in Megalingual's tree, a broader language in its first variety.",
  ):
    assertEquals(
      best(
        Language.German,
        "en-US",
        "de-CH",
        "de-AT",
        "de-DE",
      ),
      Some("de-DE"),
    )
    assertEquals(
      best(
        Language.German,
        "en-US",
        "de-CH",
        "de-AT",
      ),
      Some("de-AT"),
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
      best(
        Language.English,
        "en-US",
        "en-AU",
        "en-GB",
      ),
      Some("en-GB"),
    )
    assertEquals(
      best(
        Language.SimplifiedChinese,
        "zh-HK",
        "zh-TW",
        "zh-CN",
      ),
      Some("zh-CN"),
    )
    assertEquals(
      best(
        Language.SimplifiedChinese,
        "zh-HK",
        "zh-TW",
      ),
      Some("zh-TW"),
    )
    assertEquals(
      best(Language.Norwegian, "da-DK", "no-NO"),
      Some("no-NO"),
    )
    assertEquals(
      best(Language.Spanish, "es_MX"),
      Some("es_MX"),
    )

  test(
    "Of voices as near, one on the device is chosen before one online, else the first.",
  ):
    assertEquals(
      best(Language.German, "~de-DE", "de-DE"),
      Some("de-DE"),
    )
    assertEquals(
      best(Language.German, "~de-DE", "~de-de"),
      Some("~de-DE"),
    )
    assertEquals(
      best(Language.German, "de-AT", "~de-DE"),
      Some("~de-DE"),
    )

  test(
    "No voice says a word of a language that it is not alike, as Swiss German is not German, nor of a tag that names no language.",
  ):
    assertEquals(
      best(Language.French, "en-US", "de-DE"),
      None,
    )
    assertEquals(
      best(Language.SwissGerman, "de-CH", "de-DE"),
      None,
    )
    assertEquals(best(Language.German, "gsw"), None)
    assertEquals(
      best(Language.SwissGerman, "de-CH", "gsw-CH"),
      Some("gsw-CH"),
    )
    assertEquals(best(Language.German, "", "xx-YY"), None)
    assertEquals(best(Language.German), None)
