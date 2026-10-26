package com.alecdorrington.codeswitch

import cats.syntax.all.*
import com.alecdorrington.iris.{LlmModel, LlmProvider}
import com.alecdorrington.omnibrowser.{Browser, Granted, Page}
import com.alecdorrington.omnibrowser.Permission.Storage
import com.alecdorrington.omnibrowser.laminar.*
import com.raquo.laminar.api.L.*

object Popup:

  /** The popup, which Laminar renders, styled by `popup.css`. */
  def page(using Granted[Storage]): Page = LaminarPage(Popup(_).view)
    .withStylesheets("popup.css")

/**
  * The popup of the toolbar button, in which the reader chooses the language to
  * learn and the model to translate with, and gives an API key for its
  * provider. Each change is kept in `browser`'s storage, and clears the problem
  * shown.
  */
private final class Popup(browser: Browser)(using Granted[Storage]):

  private val storage  = browser.storage
  private val model    = browser.signal(Translator.model)
  private val provider = model.map(_.provider)

  def view: HtmlElement = div(
    headerTag(
      img(src := "icons/icon-32.png", alt := ""),
      h1(Codeswitch.name),
    ),
    label(span("Language to learn"), languages),
    label(span("Model"), models),
    label(
      span(
        idAttr := "key-name",
        child.text <-- provider.map(keyName),
      ),
      key,
    ),
    p(
      cls := "note",
      "The key is kept on this device, and sent only to its provider, with the words to translate.",
    ),
    p(
      cls    := "problem",
      idAttr := "problem",
      child.text <-- browser.signal(Codeswitch.problem),
    ),
    // Each change, as its input event bubbles up, may solve the problem shown.
    onInput --> browser.observer(_ => dismiss),
  )

  private def languages = select(
    idAttr := "language",
    option(value := "", "None"),
    Language
      .values
      .map(language => option(value := language.code, language.name)),
    value <-- browser.signal(Codeswitch.language).map(_.fold("")(_.code)),
    onChange.mapToValue.map(Language.parse) -->
      browser.writer(Codeswitch.language),
  )

  /** The models, by provider, each named as its provider names it. */
  private def models = select(
    idAttr := "model",
    Translator
      .models
      .map((provider, models) =>
        optGroup(
          labelAttr := provider.displayName,
          models.map(model => option(value := model.name, caption(model))),
        ),
      ),
    value <-- model.map(_.name),
    onChange
      .mapToValue
      .map(LlmModel.parse)
      .collect { case Some(model) => model } -->
      browser.writer(Translator.model),
  )

  private def caption(model: LlmModel) =
    if model == Translator.defaultModel then s"${ model.id } (quick)"
    else model.id

  /** The API key of the chosen model's provider, kept apart from the others'. */
  private def key = input(
    idAttr       := "key",
    typ          := "password",
    autoComplete := "off",
    spellCheck   := false,
    value <--
      provider
        .combineWith(browser.signal(Codeswitch.apiKeys))
        .map((provider, keys) => keys.getOrElse(provider.key, "")),
    // On each input, as the popup may close before the field loses focus.
    onInput.mapToValue.compose(_.withCurrentValueOf(provider)) -->
      browser.observer[(String, LlmProvider)]((key, provider) =>
        storage
          .update(Codeswitch.apiKeys)(_.updated(provider.key, key.trim))
          .void,
      ),
  )

  private def keyName(provider: LlmProvider) =
    s"${ provider.displayName } API key"

  /**
    * Clears the problem shown, and the mark on the toolbar button, both of
    * every tab and of the tab that the popup was opened over.
    */
  private def dismiss = storage.set(Codeswitch.problem, "") *>
    Codeswitch.mark(browser.action, None, "") *>
    browser
      .tabs
      .active
      .flatMap(_.traverse_(tab =>
        Codeswitch.mark(browser.action, Some(tab.id), ""),
      ))
