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

  private val words = List(Word(
    "cat",
    "the [[cat]] sat",
    Language.French,
  ))

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
    val codec = Codeswitch.native.codec
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
    assertEquals(
      codec.decodeJson(Json.fromString("no")),
      Right(None),
    )
    assertEquals(
      Codec[Level].apply(Level.B2),
      Json.fromString("B2"),
    )
    keeps(Codec[Level], Level.values.toList)

  test(
    "The languages the reader reads fluently are kept by their codes, and those they learn with their levels, and any that cannot be read, as of a code no language has, is left out, not a failure.",
  ):
    val fluent = Codeswitch.fluent.codec
    assertEquals(
      fluent(List(
        Language.French,
        Language.BritishEnglish,
      )),
      Json.arr(
        Json.fromString("fr"),
        Json.fromString("en-GB"),
      ),
    )
    keeps(
      fluent,
      List(Nil, Language.values.toList),
    )
    assertEquals(
      fluent.decodeJson(Json.arr(
        Json.fromString("fr"),
        Json.fromString("no"),
        Json.fromInt(1),
      )),
      Right(List(Language.French)),
    )
    val learning = Codeswitch.learning.codec
    assertEquals(
      learning(List(Learnt(Language.German, Level.B1))),
      Json.arr(Json.obj(
        "language" -> Json.fromString("de"),
        "level"    -> Json.fromString("B1"),
      )),
    )
    keeps(
      learning,
      List(Level.values.toList.map(Learnt(Language.Japanese, _))),
    )
    assertEquals(
      learning.decodeJson(Json.arr(
        Json.obj(
          "language" -> Json.fromString("no"),
          "level"    -> Json.fromString("B1"),
        ),
        Json.obj(
          "language" -> Json.fromString("de"),
          "level"    -> Json.fromString("Z9"),
        ),
        Json.obj(
          "language" -> Json.fromString("fr"),
          "level"    -> Json.fromString("A2"),
        ),
      )),
      Right(List(Learnt(Language.French, Level.A2))),
    )

  test(
    "A word a level above the reader's is explained, a harder one glossed, and any other left alone, and nothing is read at C2, nor in a language whose words are not spaced.",
  ):
    val learnt = Learnt(Language.German, Level.B1)
    assertEquals(
      Level.values.toList.map(learnt.kind),
      List(
        None,
        None,
        None,
        Some(Hint.Kind.Explained),
        Some(Hint.Kind.Glossed),
        Some(Hint.Kind.Glossed),
      ),
    )
    assert(learnt.reads)
    assert(!learnt.copy(level = Level.C2).reads)
    assert(!learnt.copy(language = Language.Japanese).reads)
    assert(!learnt.copy(language = Language.Thai).reads)
    assert(!learnt.copy(language = Language.Cantonese).reads)

  test(
    "Passages in the reader's languages, or in none known, have words shown in those being learnt, which have them marked instead, and those in any other are left alone, as is everything while nothing is learnt.",
  ):
    import Language.*
    val learner = Learner.chosen(
      Some(BritishEnglish),
      List(French),
      List(
        Learnt(German, Level.B1),
        Learnt(Japanese, Level.A2),
      ),
      Foreign.Leave,
      Nil,
    )
    assert(learner.active)
    assertEquals(
      List(
        Some(AmericanEnglish),
        Some(French),
        None,
        Some(SwissGerman),
        Some(Japanese),
        Some(Italian),
      ).map(learner.switches),
      List(true, true, true, false, false, false),
    )
    assertEquals(
      List(SwissGerman, Japanese, Italian).map(learner.learnt),
      List(
        Some(Learnt(GermanGerman, Level.B1)),
        Some(Learnt(Japanese, Level.A2)),
        None,
      ),
    )
    assertEquals(
      List(0.0, 0.49, 0.5, 0.99, 1.0).map(learner.into),
      List(
        GermanGerman,
        GermanGerman,
        Japanese,
        Japanese,
        Japanese,
      ),
    )
    assert(!learner.translates(Some(Italian)))
    val idle = learner.copy(learning = Nil)
    assert(!idle.active)
    assert(!idle.switches(Some(French)))
    assert(!idle.switches(None))

  test(
    "Passages in a foreign language, neither read fluently nor learnt, are translated whole, unless the reader would leave them, but not those in none known, and it is all a page may need.",
  ):
    import Language.*
    val learner = Learner.chosen(
      Some(BritishEnglish),
      List(French),
      List(Learnt(German, Level.B1)),
      Foreign.Translate,
      Nil,
    )
    assertEquals(
      List(
        Some(Italian),
        Some(BrazilianPortuguese),
        Some(AmericanEnglish),
        Some(French),
        Some(SwissGerman),
        None,
      ).map(learner.translates),
      List(true, true, false, false, false, false),
    )
    val translating = learner.copy(learning = Nil)
    assert(translating.active)
    assert(translating.translates(Some(Italian)))
    assert(!translating.switches(Some(AmericanEnglish)))
    assert(!translating.switches(None))
    assert(!learner.copy(foreign = Foreign.Leave).translates(Some(Italian)))
    assertEquals(
      Codec[Foreign].apply(Foreign.Leave),
      Json.fromString("Leave"),
    )
    keeps(Codec[Foreign], Foreign.values.toList)

  test(
    "The reader's choices are taken as learnable varieties, each once, their own language first, and none fluent alike it, and until they choose it, it is the browser's first not being learnt.",
  ):
    import Language.*
    val learner = Learner.chosen(
      None,
      List(French, AmericanEnglish, French),
      List(
        Learnt(German, Level.B1),
        Learnt(SwissGerman, Level.C1),
        Learnt(Spanish, Level.A1),
      ),
      Foreign.Translate,
      List(German, English),
    )
    assertEquals(learner.native, BritishEnglish)
    assertEquals(
      learner.fluent.toList,
      List(BritishEnglish, French),
    )
    assertEquals(
      learner.learning,
      List(
        Learnt(GermanGerman, Level.B1),
        Learnt(EuropeanSpanish, Level.A1),
      ),
    )
    assertEquals(
      Learner
        .chosen(
          Some(Portuguese),
          Nil,
          Nil,
          Foreign.Translate,
          List(German),
        )
        .fluent
        .toList,
      List(BrazilianPortuguese),
    )

  test(
    "Until the reader chooses, they read only their own language, which is not chosen, learn none, have pages in any other translated, and Claude Haiku translates.",
  ):
    for
      browser  <- FakeBrowser.install(Codeswitch)
      native   <- browser.stored(Codeswitch.native)
      fluent   <- browser.stored(Codeswitch.fluent)
      learning <- browser.stored(Codeswitch.learning)
      foreign  <- browser.stored(Codeswitch.foreign)
      model    <- browser.stored(Translator.model)
    yield
      assertEquals(native, None)
      assertEquals((fluent, learning), (Nil, Nil))
      assertEquals(foreign, Foreign.Translate)
      assertEquals(model, LlmModel.ClaudeHaiku4_5)

  test(
    "With no API key, no hints are found, and nothing is looked up where there is nothing to look up, and the reader is told why.",
  ):
    for
      browser <- FakeBrowser.install(Codeswitch)
      _       <- browser.store(
        Codeswitch.learning,
        List(Learnt(Language.German, Level.B1)),
      )
      hinted <- browser
        .call(Codeswitch.hints)(Passages(
          Learnt(Language.German, Level.B1),
          List("Das Haus."),
        ))
        .attempt
      badge <- browser.badge
      blank <- browser
        .call(Codeswitch.define)(Lookup(" ", "noun", "bank", Language.German))
        .attempt
    yield
      assert(
        hinted
          .left
          .exists(_.getMessage.contains("Add an API key from Anthropic.")),
        hinted,
      )
      assertEquals(badge, "!")
      assert(
        blank
          .left
          .exists(_.getMessage.contains("There is nothing to look up.")),
        blank,
      )

  test(
    "Passages are translated whole only while the reader has foreign pages translated, and otherwise none of them, unasked.",
  ):
    val passages = List(List("Il ponte ", "vecchio", "."))
    for
      browser <- FakeBrowser.install(Codeswitch)
      asked   <- browser.call(Codeswitch.render)(passages).attempt
      _       <- browser.store(Codeswitch.foreign, Foreign.Leave)
      left    <- browser.call(Codeswitch.render)(passages)
    yield
      assertEquals(
        asked,
        Left(MessagingError(
          "render",
          "Add an API key from Anthropic.",
        )),
      )
      assertEquals(left, List(Nil))

  test(
    "A page cannot have words translated into, passages read in, or words looked up in a language not being learnt, which asks nothing, and so needs no key, but one that is is asked for.",
  ):
    for
      browser <- FakeBrowser.install(Codeswitch)
      _       <- browser.store(
        Codeswitch.learning,
        List(Learnt(Language.German, Level.B1)),
      )
      declined <- browser.call(Codeswitch.translate)(List(
        Word(
          "cat",
          "the [[cat]] sat",
          Language.Japanese,
        ),
        Word(
          "dog",
          "the [[dog]] ran",
          Language.Italian,
        ),
      ))
      unread <- browser.call(Codeswitch.hints)(Passages(
        Learnt(Language.Japanese, Level.A1),
        List("猫です。", "犬です。"),
      ))
      refused <- browser
        .call(Codeswitch.define)(
          Lookup("Bank", "noun", "bank", Language.Italian),
        )
        .attempt
      asked <- browser
        .call(Codeswitch.translate)(List(
          Word(
            "cat",
            "the [[cat]] sat",
            Language.Japanese,
          ),
          Word(
            "dog",
            "the [[dog]] ran",
            Language.SwissGerman,
          ),
        ))
        .attempt
    yield
      assertEquals(
        declined,
        List(
          Translation.declined,
          Translation.declined,
        ),
      )
      assertEquals(unread, List(Nil, Nil))
      assert(
        refused
          .left
          .exists(
            _.getMessage
              .contains(
                "Only words of the languages you are learning are looked up.",
              ),
          ),
        refused,
      )
      assertEquals(
        asked,
        Left(problem("Add an API key from Anthropic.")),
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

  test("The background may reach Kaikki and the API of every provider."):
    assertEquals(
      Codeswitch.hosts,
      Seq(
        "https://kaikki.org/*",
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
    val passages = Passages(
      Learnt(Language.German, Level.B1),
      List("Das Haus.", "Die Brücke."),
    )
    val found = List(
      List(Hint(
        "Brücke",
        "bridge",
        "bridge",
        Hint.Kind.Glossed,
        "Brücke",
        Language.German,
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
    "A failure to translate passages whole translates none of them, and is kept as the page's problem.",
  ):
    val passages = List(
      List("Il ponte ", "vecchio", "."),
      List("Ciao."),
    )
    val found = List(
      List("The ", "old", " bridge."),
      List("Hello."),
    )
    for
      noted  <- Ref[IO].of("")
      failed <- Codeswitch.rendering(noted)(_ =>
        IO.raiseError(MessagingError(
          "render",
          "Add an API key from Anthropic.",
        )),
      )(passages)
      kept      <- noted.get
      rendered  <- Codeswitch.rendering(noted)(_ => IO.pure(found))(passages)
      forgotten <- noted.get
    yield
      assertEquals(failed, List(Nil, Nil))
      assertEquals(kept, "Add an API key from Anthropic.")
      assertEquals(rendered, found)
      assertEquals(forgotten, "")

  test(
    "Until the reader chooses their own language, it is the first that the browser prefers of those not being learnt, in any variety, but no other language.",
  ):
    import Language.*
    import Learner.own
    assertEquals(
      own(List(German, French), List(German)),
      French,
    )
    assertEquals(
      own(
        List(German, French, Italian),
        List(French, German),
      ),
      Italian,
    )
    assertEquals(
      own(List(German), List(German)),
      BritishEnglish,
    )
    assertEquals(own(Nil, List(French)), BritishEnglish)
    assertEquals(
      own(List(Spanish, German), Nil),
      EuropeanSpanish,
    )
    assertEquals(
      own(
        List(SwissGerman, BritishEnglish),
        List(GermanGerman),
      ),
      BritishEnglish,
    )
    assert(BritishEnglish.alike(AmericanEnglish))
    assert(English.alike(AustralianEnglish))
    assert(!BrazilianPortuguese.alike(Spanish))
    assert(!Cantonese.alike(Japanese))

  test("A broader language, as stored before, reads as its first variety."):
    import Language.*
    val stored = Codeswitch.native.codec.decodeJson(Json.fromString("de"))
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
        Codeswitch.native,
        Some(Language.English),
      )
      _ <- browser.store(
        Codeswitch.learning,
        List(Learnt(Language.German, Level.B1)),
      )
      _ <- browser.store(
        Dictionary.wiktionary(Language.German, "Bank"),
        Some(Kaikki.entries("Bank")),
      )
      found <- browser.call(Codeswitch.define)(Lookup(
        " Bank ",
        "Noun",
        "bank",
        Language.German,
      ))
      kept <- browser.stored(Dictionary.kept)
    yield
      assertEquals(
        found.map(_.entry.senses.head.glosses),
        Some(List("bank (financial institution)")),
      )
      assertEquals(
        found.map(_.principal),
        Some(List("Genitive" -> "Bank", "Plural" -> "Banken")),
      )
      assertEquals(
        found.map(_.tables.map(_.title)),
        Some(List("Declension")),
      )
      assertEquals(kept, List("wiktionary:de:Bank"))

  test(
    "With no API key for the chosen model's provider, nothing is translated, and the reader is told why, as a key is used only by its own provider's models.",
  ):
    for
      browser <- FakeBrowser.install(Codeswitch)
      _       <- browser.store(
        Codeswitch.learning,
        List(Learnt(Language.French, Level.B1)),
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
