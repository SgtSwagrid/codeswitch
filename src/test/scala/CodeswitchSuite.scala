package com.alecdorrington.codeswitch

import com.alecdorrington.omnibrowser.Codec
import com.alecdorrington.omnibrowser.testkit.FakeBrowser
import munit.CatsEffectSuite

class CodeswitchSuite extends CatsEffectSuite:

  private val words = List(Word("cat", "the [[cat]] sat"))

  test("A language is kept by its code."):
    assertEquals(
      Codec[Language].encode(Language.German).toString,
      "de",
    )
    assertEquals(
      Language
        .values
        .toList
        .map(language =>
          Codec[Language].decode(Codec[Language].encode(language)),
        ),
      Language.values.toList.map(Right(_)),
    )

  test("A word is kept with its context."):
    val word = Word("cat", "the [[cat]] sat")
    assertEquals(
      Codec[Word].decode(Codec[Word].encode(word)),
      Right(word),
    )

  test(
    "With no language chosen, nothing is translated, and the reader is told why.",
  ):
    for
      browser <- FakeBrowser.install(Codeswitch)
      result  <- browser.call(Codeswitch.translate)(words).attempt
      problem <- browser.stored(Codeswitch.problem)
      badge   <- browser.badge
    yield
      assert(result.isLeft)
      assertEquals(problem, "Choose a language to learn.")
      assertEquals(badge, "!")

  test("With no API key, nothing is translated, and the reader is told why."):
    for
      browser <- FakeBrowser.install(Codeswitch)
      _       <- browser.store(
        Codeswitch.language,
        Some(Language.French),
      )
      result  <- browser.call(Codeswitch.translate)(words).attempt
      problem <- browser.stored(Codeswitch.problem)
    yield
      assert(result.isLeft)
      assertEquals(
        problem,
        "Add an API key from Anthropic, OpenAI or Gemini.",
      )

  test("With an API key of no provider known, the reader is told so."):
    for
      browser <- FakeBrowser.install(Codeswitch)
      _       <- browser.store(
        Codeswitch.language,
        Some(Language.French),
      )
      _       <- browser.store(Codeswitch.apiKey, "nonsense")
      result  <- browser.call(Codeswitch.translate)(words).attempt
      problem <- browser.stored(Codeswitch.problem)
    yield
      assert(result.isLeft)
      assertEquals(
        problem,
        "This API key is not from Anthropic, OpenAI or Gemini.",
      )
