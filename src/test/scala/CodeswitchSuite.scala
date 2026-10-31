package com.alecdorrington.codeswitch

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.alecdorrington.codeswitch.Translator.given
import com.alecdorrington.iris.LlmModel
import com.alecdorrington.megalingual.Language
import com.alecdorrington.omnibrowser.{MessagingError, StorageKey}
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
        Some(SwissStandardGerman),
        Some(Japanese),
        Some(Italian),
      ).map(learner.switches),
      List(true, true, true, false, false, false),
    )
    assertEquals(
      List(SwissStandardGerman, Japanese, Italian).map(learner.learnt),
      List(
        Some(Learnt(German, Level.B1)),
        Some(Learnt(Japanese, Level.A2)),
        None,
      ),
    )
    assertEquals(
      List(0.0, 0.49, 0.5, 0.99, 1.0).map(learner.into),
      List(
        German,
        German,
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
        Some(SwissStandardGerman),
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
    "The reader's choices are taken as they are, a broader language or a variety, but Chinese, in no script, as Simplified Chinese, each once in any variety, the first kept, their own language first, and none fluent alike it, and until they choose it, it is the browser's first not being learnt.",
  ):
    import Language.*
    val learner = Learner.chosen(
      None,
      List(French, AmericanEnglish, French),
      List(
        Learnt(German, Level.B1),
        Learnt(SwissStandardGerman, Level.C1),
        Learnt(EuropeanSpanish, Level.A1),
        Learnt(Chinese, Level.A2),
      ),
      Foreign.Translate,
      List(German, English),
    )
    assertEquals(learner.native, English)
    assertEquals(
      learner.fluent.toList,
      List(English, French),
    )
    assertEquals(
      learner.learning,
      List(
        Learnt(German, Level.B1),
        Learnt(EuropeanSpanish, Level.A1),
        Learnt(SimplifiedChinese, Level.A2),
      ),
    )
    assertEquals(
      Learner
        .chosen(
          Some(Chinese),
          List(
            BrazilianPortuguese,
            Portuguese,
            TaiwaneseChinese,
          ),
          Nil,
          Foreign.Translate,
          List(German),
        )
        .fluent
        .toList,
      List(SimplifiedChinese, BrazilianPortuguese),
    )

  test(
    "Swiss German is a language of its own, apart from German, which may be learnt beside it or by its readers, and German (Switzerland), which is both, is taken as Swiss Standard German.",
  ):
    import Language.*
    assertEquals(
      List(
        SwissGerman,
        SwissStandardGerman,
        GermanOfSwitzerland,
        GermanGerman,
        Cantonese,
        BritishEnglish,
      ).map(_.whole),
      List(
        SwissGerman,
        German,
        German,
        German,
        Chinese,
        English,
      ),
    )
    assert(!SwissGerman.alike(German))
    assert(!SwissGerman.alike(SwissStandardGerman))
    assert(SwissStandardGerman.alike(GermanGerman))
    val both = Learner.chosen(
      Some(English),
      Nil,
      List(
        Learnt(German, Level.B1),
        Learnt(SwissGerman, Level.A2),
      ),
      Foreign.Translate,
      Nil,
    )
    assertEquals(
      both.learning.map(_.language),
      List(German, SwissGerman),
    )
    assertEquals(
      List(SwissStandardGerman, SwissGerman).map(
        both.learnt(_).map(_.language),
      ),
      List(Some(German), Some(SwissGerman)),
    )
    val swiss = Learner.chosen(
      Some(GermanGerman),
      Nil,
      List(Learnt(SwissGerman, Level.A2)),
      Foreign.Translate,
      Nil,
    )
    assertEquals(
      List(
        Some(GermanGerman),
        Some(SwissStandardGerman),
        Some(SwissGerman),
      ).map(swiss.switches),
      List(true, true, false),
    )
    assertEquals(
      swiss.learnt(SwissGerman),
      Some(Learnt(SwissGerman, Level.A2)),
    )
    assert(!swiss.translates(Some(SwissGerman)))
    assertEquals(
      learnable.resolve(GermanOfSwitzerland),
      SwissStandardGerman,
    )
    assert(learnable.listed.contains(GermanOfSwitzerland))
    assertEquals(
      Learner.own(
        List(GermanOfSwitzerland),
        List(SwissGerman),
      ),
      SwissStandardGerman,
    )
    assertEquals(
      Learner.own(
        List(GermanOfSwitzerland),
        List(SwissStandardGerman),
      ),
      English,
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
            Language.SwissStandardGerman,
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
    "Until the reader chooses their own language, it is the first that the browser prefers of those not being learnt, in any variety, as the browser names it, but Chinese as Simplified Chinese, or else English.",
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
      English,
    )
    assertEquals(own(Nil, List(French)), English)
    assertEquals(own(List(Spanish, German), Nil), Spanish)
    assertEquals(
      own(List(Chinese), Nil),
      SimplifiedChinese,
    )
    assertEquals(
      own(List(TaiwaneseChinese), Nil),
      TaiwaneseChinese,
    )
    assertEquals(
      own(
        List(SwissStandardGerman, BritishEnglish),
        List(GermanGerman),
      ),
      BritishEnglish,
    )
    assert(BritishEnglish.alike(AmericanEnglish))
    assert(English.alike(AustralianEnglish))
    assert(!BrazilianPortuguese.alike(Spanish))
    assert(!Cantonese.alike(Japanese))

  test(
    "A passage's language is the one the browser found, if it is one known, or else the one the page says, and one in none takes the language of most of the page's words found in the pass.",
  ):
    import Language.*
    assertEquals(Language.fromTag("xx"), None)
    assertEquals(
      foundLanguage(Some("fr"), Some(German)),
      Some(French),
    )
    assertEquals(
      foundLanguage(Some("xx"), Some(German)),
      Some(German),
    )
    assertEquals(
      foundLanguage(None, Some(German)),
      Some(German),
    )
    assertEquals(foundLanguage(None, None), None)
    val tally = Tally
      .empty
      .add(1, German, 30)
      .add(1, English, 4)
      .add(1, German, 2)
    assertEquals(tally.prevailing(1), Some(German))
    assertEquals(tally.prevailing(2), None)
    assertEquals(
      tally.add(2, English, 1).prevailing(2),
      Some(English),
    )
    assertEquals(Tally.empty.prevailing(-1), None)

  test(
    "A passage not yet looked at in a pass, as one the page put in meanwhile, has none of its words switched until it is, and a page is read whole only if anything would come of it.",
  ):
    import Language.*
    val learner = Learner.chosen(
      Some(BritishEnglish),
      Nil,
      List(
        Learnt(German, Level.C2),
        Learnt(Japanese, Level.B1),
      ),
      Foreign.Leave,
      Nil,
    )
    assertEquals(
      List(
        None,
        Some(None),
        Some(Some(AmericanEnglish)),
        Some(Some(Italian)),
      ).map(learner.switchesFound),
      List(false, true, true, false),
    )
    assert(!learner.busy(0))
    assert(learner.busy(0.05))
    assert(learner.copy(learning = List(Learnt(German, Level.B1))).busy(0))
    assert(!learner.copy(learning = Nil).busy(0.4))
    assert(learner.copy(foreign = Foreign.Translate).busy(0))

  test("The settings that earlier releases kept are taken out."):
    val retired = List(
      "language",
      "level",
      "translating",
      "reading",
    ).map(StorageKey.sync[Json](_, Json.Null))
    for
      browser <- FakeBrowser.install(Codeswitch)
      _       <- retired.traverse_(browser.store(_, Json.fromString("de")))
      kept    <- retired.traverse(browser.stored(_))
      _       <- browser.restart
      left    <- retired.traverse(browser.stored(_))
    yield
      assertEquals(
        kept,
        retired.map(_ => Json.fromString("de")),
      )
      assertEquals(left, retired.map(_ => Json.Null))

  test(
    "Every language may be learnt, a broader one as itself, but Chinese, in no script, and German (Switzerland), both a standard and dialects, only as one of their varieties, the first unless one is chosen, though the pickers still offer them, their varieties beneath them.",
  ):
    import Language.*
    assertEquals(
      Language.values.toList.filterNot(learnable.supports),
      List(Chinese, GermanOfSwitzerland),
    )
    assertEquals(
      learnable.listed,
      Language.values.toList,
    )
    assertEquals(
      List(
        Chinese,
        GermanOfSwitzerland,
        German,
        GermanGerman,
        French,
      ).map(learnable.resolve),
      List(
        SimplifiedChinese,
        SwissStandardGerman,
        German,
        GermanGerman,
        French,
      ),
    )

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

  test(
    "The background keeps each rating and sighting of a word of a language being learnt, but none of another, with the sum of every word's familiarity, which is counted afresh once the reader's levels change.",
  ):
    def headword
      (
        word: String,
        level: Option[Level],
        language: Language,
      ) = Headword(
      word,
      language,
      "noun",
      "meaning",
      "meaning",
      level,
    )
    val german = Language.German
    for
      browser <- FakeBrowser.install(Codeswitch)
      _       <- browser.store(
        Codeswitch.learning,
        List(Learnt(Language.SwissStandardGerman, Level.B1)),
      )
      _ <- browser.call(Codeswitch.seen)(List(
        headword("Bahnhof", Some(Level.C1), german),
        headword("Gleis", Some(Level.B2), german),
        headword("Haus", None, german),
        headword(
          "casa",
          Some(Level.A2),
          Language.Italian,
        ),
      ))
      _ <- browser.call(Codeswitch.rate)(Rating(
        "Gleis",
        german,
        Some(Level.B2),
        Answer.Knew,
      ))
      _ <- browser.call(Codeswitch.rate)(Rating(
        "Zug",
        german,
        Some(Level.B2),
        Answer.Didnt,
      ))
      _ <- browser.call(Codeswitch.rate)(Rating(
        "casa",
        Language.Italian,
        None,
        Answer.Knew,
      ))
      first <- browser.stored(Familiarity.total)
      gleis <- browser.stored(Familiarity.key(german, "Gleis"))
      casa  <- browser.stored(Familiarity.seenKey(Language.Italian, "casa"))
      rated <- browser.stored(Familiarity.key(Language.Italian, "casa"))
      _     <- browser.store(
        Codeswitch.learning,
        List(Learnt(Language.SwissStandardGerman, Level.B2)),
      )
      _ <- browser.call(Codeswitch.seen)(List(
        headword("Bahnhof", Some(Level.C1), german),
      ))
      second <- browser.stored(Familiarity.total)
    yield
      val gleisRated = Familiarity.after(0.5, Answer.Knew)
      val zugRated   = Familiarity.after(0.5, Answer.Didnt)
      assertEquals(gleis, Some(gleisRated))
      assertEquals((casa, rated), (None, None))
      assertEqualsDouble(
        first.map(_.sum).getOrElse(-1.0),
        0.2 + gleisRated + 0.5 + zugRated,
        1e-9,
      )
      assertEqualsDouble(
        second.map(_.sum).getOrElse(-1.0),
        0.5 + gleisRated + 0.5 + zugRated,
        1e-9,
      )
