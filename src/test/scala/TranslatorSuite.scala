package com.alecdorrington.codeswitch

import cats.effect.IO
import com.alecdorrington.iris.{LlmClient, LlmError, LlmModel, LlmProvider}
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
    Translator.config("sk-ant-key").get,
    BackendStub(CatsMonadAsyncError[IO]())
      .whenAnyRequest
      .thenRespond(ResponseStub.adjust(body, status)),
  )

  test("An API key's provider is known from how it begins."):
    assertEquals(
      List(
        "sk-ant-1",
        "sk-proj-1",
        "AIza1",
        " sk-ant-1 ",
        "nonsense",
      ).map(Translator.config(_).map(_.provider)),
      List(
        Some(LlmProvider.Anthropic),
        Some(LlmProvider.OpenAi),
        Some(LlmProvider.Gemini),
        Some(LlmProvider.Anthropic),
        None,
      ),
    )

  test("Anthropic's keys are used with its quick model."):
    assertEquals(
      Translator.config("sk-ant-1").map(_.model),
      Some(LlmModel.ClaudeHaiku4_5),
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
        """Sure: ["chat", "Paris"] done.""",
        words,
      ),
      Right(List(Some("chat"), None)),
    )

  test("A word given no translation, or an empty one, has none."):
    assertEquals(
      Translator.translations("""[" "]""", words),
      Right(List(None, None)),
    )

  test("A reply with no array of translations is a problem."):
    assert(Translator.translations("I cannot help.", words).isLeft)

  test("A client's translations are those of its reply."):
    Translator
      .translate(
        client(anthropic("""["chat", "Paris"]""")),
        Language.French,
        words,
      )
      .assertEquals(List(Some("chat"), None))

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
