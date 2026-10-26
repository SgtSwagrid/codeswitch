package com.alecdorrington.codeswitch

import com.alecdorrington.iris.LlmModel
import com.alecdorrington.omnibrowser.Target
import com.alecdorrington.omnibrowser.testkit.FakeBrowser
import munit.CatsEffectSuite

/**
  * Checks the popup, both as its code runs within the test, and as the plugin
  * built it for each browser.
  */
class PopupSuite extends CatsEffectSuite:

  private val claude = LlmModel.ClaudeHaiku4_5

  private val gpt = LlmModel.Gpt5Mini

  private val installs = Seq(
    "within the test" -> FakeBrowser.install(Codeswitch),
  ) ++
    Target
      .values
      .map(target =>
        s"as built for ${ target.toString }" -> FakeBrowser.installBuilt(target),
      )

  installs.foreach: (name, install) =>

    test(
      s"The popup shows the settings, with the key of the model's provider, $name.",
    ):
      for
        browser <- install
        _       <- browser.store(
          Codeswitch.language,
          Some(Language.German),
        )
        _ <- browser.store(Translator.model, gpt)
        _ <- browser.store(
          Codeswitch.apiKeys,
          Map(
            claude.provider.key -> "sk-ant-1",
            gpt.provider.key    -> "sk-1",
          ),
        )
        popup    <- browser.openPopup
        language <- popup.value("#language")
        model    <- popup.value("#model")
        keyName  <- popup.text("#key-name")
        key      <- popup.value("#key")
      yield
        assertEquals(language, "de")
        assertEquals(model, gpt.name)
        assertEquals(keyName, "OpenAI API key")
        assertEquals(key, "sk-1")

    test(s"Until the reader chooses, the popup shows the defaults, $name."):
      for
        browser  <- install
        popup    <- browser.openPopup
        language <- popup.value("#language")
        model    <- popup.value("#model")
        keyName  <- popup.text("#key-name")
        key      <- popup.value("#key")
      yield
        assertEquals(language, "")
        assertEquals(model, claude.name)
        assertEquals(keyName, "Anthropic API key")
        assertEquals(key, "")

    test(s"The language chosen is kept, $name."):
      for
        browser <- install
        popup   <- browser.openPopup
        _       <- popup.enter("#language", "fr")
        chosen  <- browser.stored(Codeswitch.language)
        _       <- popup.enter("#language", "")
        cleared <- browser.stored(Codeswitch.language)
      yield
        assertEquals(chosen, Some(Language.French))
        assertEquals(cleared, None)

    test(
      s"Choosing a model of another provider shows that provider's key, $name.",
    ):
      for
        browser <- install
        _       <- browser.store(
          Codeswitch.apiKeys,
          Map(
            claude.provider.key -> "sk-ant-1",
            gpt.provider.key    -> "sk-1",
          ),
        )
        popup   <- browser.openPopup
        before  <- popup.value("#key")
        _       <- popup.enter("#model", gpt.name)
        model   <- browser.stored(Translator.model)
        keyName <- popup.text("#key-name")
        after   <- popup.value("#key")
      yield
        assertEquals(before, "sk-ant-1")
        assertEquals(model, gpt)
        assertEquals(keyName, "OpenAI API key")
        assertEquals(after, "sk-1")

    test(s"Each key entered is kept for its own provider, $name."):
      for
        browser <- install
        popup   <- browser.openPopup
        _       <- popup.enter("#key", " sk-ant-1 ")
        _       <- popup.enter("#model", gpt.name)
        blank   <- popup.value("#key")
        _       <- popup.enter("#key", "sk-1")
        keys    <- browser.stored(Codeswitch.apiKeys)
      yield
        assertEquals(blank, "")
        assertEquals(
          keys,
          Map(
            claude.provider.key -> "sk-ant-1",
            gpt.provider.key    -> "sk-1",
          ),
        )

    test(
      s"The popup shows why translating failed, until a change clears it and the mark, $name.",
    ):
      for
        browser <- install
        _       <- browser
          .call(Codeswitch.translate)(List(Word("cat", "the [[cat]] sat")))
          .attempt
        popup   <- browser.openPopup
        shown   <- popup.text("#problem")
        _       <- popup.enter("#language", "fr")
        problem <- browser.stored(Codeswitch.problem)
        cleared <- popup.text("#problem")
        badge   <- browser.badge
      yield
        assertEquals(shown, "Choose a language to learn.")
        assertEquals(problem, "")
        assertEquals(cleared, "")
        assertEquals(badge, "")
