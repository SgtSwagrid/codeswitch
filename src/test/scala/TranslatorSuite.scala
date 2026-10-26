package com.alecdorrington.codeswitch

import cats.effect.IO
import com.alecdorrington.codeswitch.Translator.given
import com.alecdorrington.iris.{LlmClient, LlmError, LlmModel, LlmProvider}
import com.alecdorrington.omnibrowser.Codec
import io.circe.Json
import io.circe.parser.parse
import munit.CatsEffectSuite
import sttp.client4.impl.cats.CatsMonadAsyncError
import sttp.client4.testing.{BackendStub, ResponseStub}
import sttp.model.StatusCode

class TranslatorSuite extends CatsEffectSuite:

  private val words = List(
    Word("cat", "the [[cat]] sat"),
    Word("Paris", "in [[Paris]] today"),
  )

  private def anthropic(text: String): String = Json
    .obj(
      "content" -> Json.arr(Json.obj(
        "type" -> Json.fromString("text"),
        "text" -> Json.fromString(text),
      )),
      "stop_reason" -> Json.fromString("end_turn"),
      "usage"       -> Json.obj(
        "input_tokens"  -> Json.fromInt(10),
        "output_tokens" -> Json.fromInt(5),
      ),
    )
    .noSpaces

  private def client
    (
      body: String,
      status: StatusCode = StatusCode.Ok,
    ) = LlmClient[IO](
    Translator.config(LlmModel.ClaudeHaiku4_5, "key"),
    BackendStub(CatsMonadAsyncError[IO]())
      .whenAnyRequest
      .thenRespond(ResponseStub.adjust(body, status)),
  )

  test("Every model Iris lists is offered once, under its provider."):
    val offered = Translator
      .models
      .flatMap((provider, models) => models.map(model => provider -> model))
    assertEquals(
      offered.map(_._2),
      LlmModel.Listed.values.toList,
    )
    assert(offered.forall((provider, model) => model.provider == provider))

  test("A model is kept by its name."):
    val codec = Codec[LlmModel]
    assertEquals(
      LlmModel
        .Listed
        .values
        .toList
        .map(model => codec.decode(codec.encode(model))),
      LlmModel.Listed.values.toList.map(Right(_)),
    )

  test("The key is given to the model's provider as it is, but trimmed."):
    val config = Translator.config(LlmModel.Gpt5Mini, " sk-key ")
    assertEquals(
      (config.provider, config.apiKey),
      (LlmProvider.OpenAi, "sk-key"),
    )

  test("The words are sent as pairs of each word and its context."):
    val sent = Translator.chat(Language.French, words).messages.last.text
    assertEquals(
      parse(sent),
      Right(Json.arr(
        Json.arr(
          Json.fromString("cat"),
          Json.fromString("the [[cat]] sat"),
        ),
        Json.arr(
          Json.fromString("Paris"),
          Json.fromString("in [[Paris]] today"),
        ),
      )),
    )

  test("The model is told which language to translate into."):
    val system = Translator.chat(Language.French, words).system
    assert(
      system.exists(_.contains("French")),
      system,
    )

  test("The translations are read from the array in the reply."):
    assertEquals(
      Translator.translations(
        """Sure: [["chat"], []] done.""",
        words,
      ),
      Right(List(
        Translation("chat"),
        Translation.declined,
      )),
    )

  test("Each piece of a translation is read with where it is shown."):
    assertEquals(
      Translator.translations(
        """[[{"text": "le chat", "replacing": "the cat"}],
          | [{"text": "à", "after": "in"}, {"text": "Paris", "before": "today"}]]"""
          .stripMargin,
        words,
      ),
      Right(List(
        Translation(List(Piece("le chat", Place.Replacing("the cat")))),
        Translation(List(
          Piece("à", Place.After("in")),
          Piece("Paris", Place.Before("today")),
        )),
      )),
    )

  test("A lone piece, or none, is read as a translation."):
    assertEquals(
      Translator.translations("""["chat", null]""", words),
      Right(List(
        Translation("chat"),
        Translation.declined,
      )),
    )

  test("A word given only itself, empty pieces, or nothing, is left as it is."):
    assertEquals(
      Translator.translations("""[[" "], ["paris"]]""", words),
      Right(List(
        Translation.declined,
        Translation.declined,
      )),
    )
    assertEquals(
      Translator.translations("""[]""", words),
      Right(List(
        Translation.declined,
        Translation.declined,
      )),
    )

  test("A reply with no array of translations is a problem."):
    assert(Translator.translations("I cannot help.", words).isLeft)

  test("A client's translations are those of its reply."):
    Translator
      .translate(
        client(anthropic("""[["chat"], []]""")),
        Language.French,
        words,
      )
      .assertEquals(List(
        Translation("chat"),
        Translation.declined,
      ))

  test("A refused key is explained to the reader."):
    Translator
      .translate(
        client("{}", StatusCode.Unauthorized),
        Language.French,
        words,
      )
      .attempt
      .map(_.left.map(Translator.explain))
      .assertEquals(Left("Anthropic refused the API key."))

  test("A problem is explained as it is."):
    assertEquals(
      Translator.explain(Problem("Something.")),
      "Something.",
    )
    assertEquals(
      Translator.explain(LlmError.Malformed("Anthropic", "bad")),
      LlmError.Malformed("Anthropic", "bad").getMessage,
    )
