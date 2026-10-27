package com.alecdorrington.codeswitch

import cats.effect.IO
import com.alecdorrington.iris.{LlmClient, LlmModel}
import io.circe.Json
import munit.CatsEffectSuite
import sttp.client4.impl.cats.CatsMonadAsyncError
import sttp.client4.testing.{BackendStub, ResponseStub}

class DictionarySuite extends CatsEffectSuite:

  private val english = Learner(
    Language.German,
    Language.English,
    Level.B1,
    translating = true,
    reading = true,
  )

  private val german = english.copy(
    language = Language.Spanish,
    native = Language.German,
  )

  private val bank = Lookup("Bank", "noun", "bank")

  private val lines =
    """{"word": "Bank", "pos": "noun", "forms": [{"form": "Banken", "tags": ["plural"]}], "senses": [{"glosses": ["bank (financial institution)"], "examples": [{"text": "Ich gehe zur Bank.", "english": "I am going to the bank."}]}]}"""

  /** Kaikki, with `lines` at every address, or nothing. */
  private def kaikki(found: Option[String]): String => IO[Option[String]] =
    _ => IO.pure(found)

  /** A model that answers every request with `text`. */
  private def answering(text: String): Dictionary.Ask = [A] =>
    (use: LlmClient[IO] => IO[A]) =>
      use(LlmClient[IO](
        Translator.config(LlmModel.ClaudeHaiku4_5, "key"),
        BackendStub(CatsMonadAsyncError[IO]())
          .whenAnyRequest
          .thenRespond(ResponseStub.adjust(anthropic(text))),
      ))

  /** No model at all, as when the reader has given no key. */
  private val unasked: Dictionary.Ask =
    [A] => (_: LlmClient[IO] => IO[A]) => IO.raiseError(Problem("No key."))

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

  test(
    "For a reader whose own language is English, Wiktionary's entry is used as it is, with no model.",
  ):
    Dictionary
      .entry(
        english,
        bank,
        kaikki(Some(lines)),
        unasked,
      )
      .map(_.map(entry => (entry.senses.head.glosses, entry.source)))
      .assertEquals(Some((
        List("bank (financial institution)"),
        Source.Wiktionary(
          Wiktionary.url(Language.German, "Bank"),
          false,
        ),
      )))

  test(
    "For a reader of another language, Wiktionary's senses are put into theirs.",
  ):
    Dictionary
      .entry(
        german,
        bank,
        kaikki(Some(lines)),
        answering(
          """[{"glosses": ["Bank (Geldinstitut)"], "examples": [{"text": "Ich gehe zur Bank.", "translation": "Ich gehe zur Bank."}]}]""",
        ),
      )
      .map(_.map(entry => (entry.senses.head.glosses, entry.source)))
      .assertEquals(Some((
        List("Bank (Geldinstitut)"),
        Source.Wiktionary(
          Wiktionary.url(Language.Spanish, "Bank"),
          true,
        ),
      )))

  test("Senses that cannot be read as as many are left in English."):
    Dictionary
      .entry(
        german,
        bank,
        kaikki(Some(lines)),
        answering("[]"),
      )
      .map(_.map(_.senses.head.glosses))
      .assertEquals(Some(List("bank (financial institution)")))

  test("Where Wiktionary has no entry, the model makes one."):
    Dictionary
      .entry(
        english,
        Lookup("Ausschreibung", "noun", "tender"),
        kaikki(None),
        answering(
          """Here: {"word": "Ausschreibung", "pos": "noun", "senses": [{"glosses": ["call for tenders"], "examples": []}], "forms": [{"form": "Ausschreibungen", "tags": ["plural"]}], "ipa": "/ˈaʊ̯sˌʃʁaɪ̯bʊŋ/"}""",
        ),
      )
      .assertEquals(Some(Entry(
        "Ausschreibung",
        "noun",
        List(Sense(List("call for tenders"), Nil)),
        List(Form("Ausschreibungen", List("plural"))),
        Some("/ˈaʊ̯sˌʃʁaɪ̯bʊŋ/"),
        None,
        Source.Model,
      )))

  test("A model's entry with no senses, or that cannot be read, is none."):
    Dictionary
      .entry(
        english,
        bank,
        kaikki(None),
        answering("""{"word": "Bank"}"""),
      )
      .assertEquals(None) *>
      Dictionary
        .entry(
          english,
          bank,
          kaikki(None),
          answering("I cannot."),
        )
        .assertEquals(None)

  test(
    "An entry is kept by the languages, the part of speech and the word in its own case.",
  ):
    assertEquals(
      Dictionary.key(english, Lookup(" Weg ", "noun", "path")).name,
      "entry:de:en:noun:Weg",
    )
