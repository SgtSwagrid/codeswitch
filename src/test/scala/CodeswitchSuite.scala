package com.alecdorrington.codeswitch

import cats.effect.{IO, Ref}
import com.alecdorrington.codeswitch.Translator.given
import com.alecdorrington.iris.LlmModel
import com.alecdorrington.omnibrowser.MessagingError
import com.alecdorrington.omnibrowser.testkit.FakeBrowser
import io.circe.{Codec, Json}
import munit.CatsEffectSuite

class CodeswitchSuite extends CatsEffectSuite:

  private val words = List(Word("cat", "the [[cat]] sat"))

  /** The failure of a translation, as the content script hears of it. */
  private def problem(reason: String) = MessagingError("translate", reason)

  /** Whether `codec` gives back each of `values` that it is given. */
  private def keeps[A](codec: Codec[A], values: List[A]): Unit = assertEquals(
    values.map(value => codec.decodeJson(codec(value))),
    values.map(Right(_)),
  )

  test("A language is kept by its code, as it has been since v0.1.0."):
    assertEquals(
      Codec[Language].apply(Language.German),
      Json.fromString("de"),
    )
    keeps(
      Codec[Language],
      Language.values.toList,
    )

  test("A word is kept with its context."):
    keeps(
      Codec[Word],
      List(Word("cat", "the [[cat]] sat")),
    )

  test("A translation is kept with its pieces and where each is shown."):
    val translation = Translation(List(
      Piece("stehe", Place.Replacing("get up")),
      Piece("auf", Place.After("early")),
      Piece("x", Place.Before("y")),
      Piece("z", Place.Home),
    ))
    keeps(Codec[Translation], List(translation))

  test("Translations that cannot be matched to the words leave each as it is."):
    Codeswitch
      .recover(
        words,
        Left(problem(Problem.unmatched.message)),
      )
      .assertEquals(List(Translation.declined))

  test("Any other failure to translate is still a failure."):
    Codeswitch
      .recover(words, Left(problem("Something.")))
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

  test("A page keeps why its last translation failed, until one succeeds."):
    val failed = MessagingError(
      "translate",
      "Add an API key from Anthropic.",
    )
    val translated = List(Translation(List(Piece("chat", Place.Home))))
    for
      noted <- Ref[IO].of("")
      _ <- Codeswitch.noting(noted)(_ => IO.raiseError(failed))(words).attempt
      kept      <- noted.get
      _         <- Codeswitch.noting(noted)(_ => IO.pure(translated))(words)
      forgotten <- noted.get
    yield
      assertEquals(kept, "Add an API key from Anthropic.")
      assertEquals(forgotten, "")

  test("A page keeps why its translations could not be matched, and reads on."):
    val unmatched = problem(Problem.unmatched.message)
    for
      noted        <- Ref[IO].of("")
      translations <-
        Codeswitch.noting(noted)(_ => IO.raiseError(unmatched))(words)
      kept <- noted.get
    yield
      assertEquals(
        translations,
        List(Translation.declined),
      )
      assertEquals(kept, Problem.unmatched.message)

  test(
    "With no language chosen, nothing is translated, and the reader is told why.",
  ):
    for
      browser <- FakeBrowser.install(Codeswitch)
      result  <- browser.call(Codeswitch.translate)(words).attempt
      badge   <- browser.badge
    yield
      assertEquals(
        result,
        Left(problem("Choose a language to learn.")),
      )
      assertEquals(badge, "!")

  test("With no API key, nothing is translated, and the reader is told why."):
    for
      browser <- FakeBrowser.install(Codeswitch)
      _       <- browser.store(
        Codeswitch.language,
        Some(Language.French),
      )
      result <- browser.call(Codeswitch.translate)(words).attempt
    yield assertEquals(
      result,
      Left(problem("Add an API key from Anthropic.")),
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
      result <- browser.call(Codeswitch.translate)(words).attempt
    yield assertEquals(
      result,
      Left(problem("Add an API key from OpenAI.")),
    )

  test("Until the reader chooses one, Claude Haiku translates."):
    for
      browser <- FakeBrowser.install(Codeswitch)
      model   <- browser.stored(Translator.model)
    yield assertEquals(model, LlmModel.ClaudeHaiku4_5)
