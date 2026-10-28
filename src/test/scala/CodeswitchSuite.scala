package com.alecdorrington.codeswitch

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.alecdorrington.codeswitch.Translator.given
import com.alecdorrington.iris.LlmModel
import com.alecdorrington.megalingual.Language
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

  test(
    "A language is kept by its code, as Megalingual's codec writes it, a stored code no language has reads as none, not as a failure, and a level is kept by its name.",
  ):
    val codec = Codeswitch.language.codec
    assertEquals(
      codec(Some(Language.German)),
      Json.fromString("de"),
    )
    assertEquals(
      codec(Some(Language.BritishEnglish)),
      Json.fromString("en-GB"),
    )
    assertEquals(codec(None), Json.Null)
    keeps(
      codec,
      None :: Language.values.toList.map(Some(_)),
    )
    for key <- List(Codeswitch.language, Codeswitch.native) do
      assertEquals(
        key.codec.decodeJson(Json.fromString("no")),
        Right(None),
      )
    assertEquals(
      Codec[Level].apply(Level.B2),
      Json.fromString("B2"),
    )
    keeps(Codec[Level], Level.values.toList)

  test(
    "A word a level above the reader's is explained, a harder one glossed, and any other left alone, and nothing is read at C2, nor in a language whose words are not spaced.",
  ):
    val learner = Learner(
      Language.German,
      Language.English,
      Level.B1,
      translating = true,
      reading = true,
    )
    assertEquals(
      Level.values.toList.map(learner.kind),
      List(
        None,
        None,
        None,
        Some(Hint.Kind.Explained),
        Some(Hint.Kind.Glossed),
        Some(Hint.Kind.Glossed),
      ),
    )
    val reader = Learner(
      Language.German,
      Language.English,
      Level.B1,
      translating = false,
      reading = true,
    )
    assert(reader.reads)
    assert(!reader.copy(level = Level.C2).reads)
    assert(!reader.copy(language = Language.Japanese).reads)
    assert(!reader.copy(language = Language.Thai).reads)
    assert(!reader.copy(language = Language.Cantonese).reads)
    assert(!reader.copy(language = Language.Chinese).active)
    assert(
      reader
        .copy(
          language = Language.Chinese,
          translating = true,
        )
        .active,
    )

  test(
    "Until the reader chooses, both modes are on, for a reader at B1, whose own language is not chosen, and Claude Haiku translates.",
  ):
    for
      browser     <- FakeBrowser.install(Codeswitch)
      translating <- browser.stored(Codeswitch.translating)
      reading     <- browser.stored(Codeswitch.reading)
      level       <- browser.stored(Codeswitch.level)
      native      <- browser.stored(Codeswitch.native)
      model       <- browser.stored(Translator.model)
    yield
      assertEquals(
        (translating, reading, level),
        (true, true, Level.B1),
      )
      assertEquals(native, None)
      assertEquals(model, LlmModel.ClaudeHaiku4_5)

  test(
    "With no language chosen, no hints are found, nothing is translated and nothing is looked up, nor where there is nothing to look up, and the reader is told why.",
  ):
    for
      browser    <- FakeBrowser.install(Codeswitch)
      hinted     <- browser.call(Codeswitch.hints)(List("Das Haus.")).attempt
      badge      <- browser.badge
      translated <- browser.call(Codeswitch.translate)(words).attempt
      none       <- browser
        .call(Codeswitch.define)(Lookup("Bank", "noun", "bank"))
        .attempt
      _ <- browser.store(
        Codeswitch.language,
        Some(Language.German),
      )
      blank <- browser
        .call(Codeswitch.define)(Lookup(" ", "noun", "bank"))
        .attempt
    yield
      assert(
        hinted
          .left
          .exists(_.getMessage.contains("Choose a language to learn.")),
        hinted,
      )
      assertEquals(badge, "!")
      assertEquals(
        translated,
        Left(problem("Choose a language to learn.")),
      )
      assert(
        none.left.exists(_.getMessage.contains("Choose a language to learn.")),
        none,
      )
      assert(
        blank
          .left
          .exists(_.getMessage.contains("There is nothing to look up.")),
        blank,
      )

  test(
    "A reply that cannot be read, or matched to the words, leaves each as it is, but any other failure to translate is still a failure.",
  ):
    Problem
      .ofBatch
      .traverse(batch =>
        Codeswitch.recover(words, Left(problem(batch.message))),
      )
      .assertEquals(Problem.ofBatch.map(_ => List(Translation.declined))) *>
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

  test(
    "A page keeps why its last translation failed, until one succeeds, and why its translations could not be matched, and reads on.",
  ):
    val failed = MessagingError(
      "translate",
      "Add an API key from Anthropic.",
    )
    val translated = List(Translation(List(Piece("chat", Place.Home))))
    val unmatched  = problem(Problem.unmatched.message)
    for
      noted <- Ref[IO].of("")
      _ <- Codeswitch.noting(noted)(_ => IO.raiseError(failed))(words).attempt
      kept         <- noted.get
      _            <- Codeswitch.noting(noted)(_ => IO.pure(translated))(words)
      forgotten    <- noted.get
      translations <-
        Codeswitch.noting(noted)(_ => IO.raiseError(unmatched))(words)
      unmatchedKept <- noted.get
    yield
      assertEquals(kept, "Add an API key from Anthropic.")
      assertEquals(forgotten, "")
      assertEquals(
        translations,
        List(Translation.declined),
      )
      assertEquals(
        unmatchedKept,
        Problem.unmatched.message,
      )

  test(
    "A failure to find hints marks none in those passages, and is kept as the page's problem.",
  ):
    val passages = List("Das Haus.", "Die Brücke.")
    val found    = List(
      List(Hint(
        "Brücke",
        "bridge",
        Hint.Kind.Glossed,
        "Brücke",
      )),
      Nil,
    )
    for
      noted  <- Ref[IO].of("")
      failed <- Codeswitch.hinting(noted)(_ =>
        IO.raiseError(MessagingError(
          "hints",
          "Add an API key from Anthropic.",
        )),
      )(passages)
      kept      <- noted.get
      hinted    <- Codeswitch.hinting(noted)(_ => IO.pure(found))(passages)
      forgotten <- noted.get
    yield
      assertEquals(failed, List(Nil, Nil))
      assertEquals(kept, "Add an API key from Anthropic.")
      assertEquals(hinted, found)
      assertEquals(forgotten, "")

  test(
    "Until the reader chooses their own language, it is the first that the browser prefers of those not being learnt, in any variety, but no other language.",
  ):
    import Language.*
    import Learner.own
    assertEquals(
      own(List(German, French), Some(German)),
      French,
    )
    assertEquals(
      own(List(German), Some(German)),
      BritishEnglish,
    )
    assertEquals(own(Nil, Some(French)), BritishEnglish)
    assertEquals(
      own(List(Spanish, German), None),
      EuropeanSpanish,
    )
    assertEquals(
      own(
        List(SwissGerman, BritishEnglish),
        Some(GermanGerman),
      ),
      BritishEnglish,
    )
    assert(BritishEnglish.alike(AmericanEnglish))
    assert(English.alike(AustralianEnglish))
    assert(!BrazilianPortuguese.alike(Spanish))
    assert(!Cantonese.alike(Japanese))

  test("A broader language, as stored before, reads as its first variety."):
    import Language.*
    val stored = Codeswitch.language.codec.decodeJson(Json.fromString("de"))
    assertEquals(
      stored.map(_.map(learnable.resolve)),
      Right(Some(GermanGerman)),
    )
    assertEquals(
      learnable.resolve(Chinese),
      SimplifiedChinese,
    )
    assertEquals(
      learnable.resolve(Portuguese),
      BrazilianPortuguese,
    )
    assertEquals(learnable.resolve(French), French)
    assert(!learnable.languages.exists(_.children.nonEmpty))

  test(
    "A word is looked up through the background, as kept, with no key for a reader whose own language is English.",
  ):
    for
      browser <- FakeBrowser.install(Codeswitch)
      _       <- browser.store(
        Codeswitch.language,
        Some(Language.German),
      )
      _ <- browser.store(
        Codeswitch.native,
        Some(Language.English),
      )
      _ <- browser.store(
        Dictionary.wiktionary(Language.German, "Bank"),
        Some(Kaikki.entries("Bank")),
      )
      found <- browser.call(Codeswitch.define)(Lookup(" Bank ", "Noun", "bank"))
      kept  <- browser.stored(Dictionary.kept)
    yield
      assertEquals(
        found.map(_.senses.head.glosses),
        Some(List("bank (financial institution)")),
      )
      assertEquals(kept, List("wiktionary:de:Bank"))

  test(
    "With no API key for the chosen model's provider, nothing is translated, and the reader is told why, as a key is used only by its own provider's models.",
  ):
    for
      browser <- FakeBrowser.install(Codeswitch)
      _       <- browser.store(
        Codeswitch.language,
        Some(Language.French),
      )
      keyless <- browser.call(Codeswitch.translate)(words).attempt
      badge   <- browser.badge
      _       <- browser.store(Translator.model, LlmModel.Gpt5Mini)
      _       <- browser.store(
        Codeswitch.apiKeys,
        Map("anthropic" -> "sk-ant-1"),
      )
      other <- browser.call(Codeswitch.translate)(words).attempt
    yield
      assertEquals(
        keyless,
        Left(problem("Add an API key from Anthropic.")),
      )
      assertEquals(badge, "!")
      assertEquals(
        other,
        Left(problem("Add an API key from OpenAI.")),
      )
