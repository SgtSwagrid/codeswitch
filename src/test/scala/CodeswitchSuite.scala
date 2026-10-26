package com.alecdorrington.codeswitch

import com.alecdorrington.codeswitch.Translator.given
import com.alecdorrington.iris.LlmModel
import com.alecdorrington.omnibrowser.{Codec, StorageArea}
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

  test("A translation is kept with its pieces and where each is shown."):
    val translation = Translation(List(
      Piece("stehe", Place.Replacing("get up")),
      Piece("auf", Place.After("early")),
      Piece("x", Place.Before("y")),
      Piece("z", Place.Home),
    ))
    assertEquals(
      Codec[Translation].decode(Codec[Translation].encode(translation)),
      Right(translation),
    )

  test("Translations that cannot be matched to the words leave each as it is."):
    Codeswitch
      .recover(words, Left(Translator.unmatched))
      .assertEquals(List(Translation.declined))

  test("Any other failure to translate is still a failure."):
    Codeswitch
      .recover(words, Left(Problem("Something.")))
      .attempt
      .map(_.isLeft)
      .assertEquals(true)

  test("The background may reach the API of every provider."):
    assertEquals(
      Codeswitch.hosts,
      Seq(
        "https://api.anthropic.com/*",
        "https://api.openai.com/*",
        "https://generativelanguage.googleapis.com/*",
      ),
    )

  test("Why translating failed is forgotten once the browser closes."):
    assertEquals(
      Codeswitch.problem.area,
      StorageArea.Session,
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
        "Add an API key from Anthropic.",
      )

  test("A key is used only by its own provider's models."):
    for
      browser <- FakeBrowser.install(Codeswitch)
      _       <- browser.store(
        Codeswitch.language,
        Some(Language.French),
      )
      _ <- browser.store(Translator.model, LlmModel.Gpt5Mini)
      _ <- browser.store(
        Codeswitch.apiKeys,
        Map("anthropic" -> "sk-ant-1"),
      )
      result  <- browser.call(Codeswitch.translate)(words).attempt
      problem <- browser.stored(Codeswitch.problem)
    yield
      assert(result.isLeft)
      assertEquals(problem, "Add an API key from OpenAI.")

  test("Until the reader chooses one, Claude Haiku translates."):
    for
      browser <- FakeBrowser.install(Codeswitch)
      model   <- browser.stored(Translator.model)
    yield assertEquals(model, LlmModel.ClaudeHaiku4_5)
