package com.alecdorrington.codeswitch

import cats.effect.IO
import cats.syntax.all.*
import com.alecdorrington.iris.{LlmModel, LlmProvider}
import com.alecdorrington.omnibrowser.{Browser, Granted, Page}
import com.alecdorrington.omnibrowser.Permission.Storage
import com.alecdorrington.omnibrowser.laminar.*
import com.raquo.laminar.api.L.*

/**
  * The popup of the toolbar button, in which the reader chooses the language to
  * learn and the model to translate with, and gives an API key for its
  * provider.
  */
object Popup:

  /** The popup, which Laminar renders, styled by `popup.css`. */
  def page(using Granted[Storage]): Page = LaminarPage(settings(_))
    .withStylesheets("popup.css")

  /**
    * The settings, as the browser stores them, each change to which is kept
    * there, and clears the problem shown, which the change may have solved.
    */
  private def settings(browser: Browser)(using Granted[Storage]): HtmlElement =
    val model    = browser.signal(Translator.model)
    val provider = model.map(_.provider)
    div(
      headerTag(
        img(src := "icons/icon-32.png", alt := ""),
        h1(Codeswitch.name),
      ),
      label(
        span("Language to learn"),
        languages(browser),
      ),
      label(span("Model"), models(browser, model)),
      label(
        span(
          idAttr := "key-name",
          child.text <-- provider.map(keyName),
        ),
        keyField(browser, provider),
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
    )

  private def languages(browser: Browser)(using Granted[Storage]): HtmlElement =
    chooser(
      browser,
      "language",
      (None +: Language.values.toSeq.map(Some(_))).map(language =>
        option(
          value := codeOf(language),
          language.fold("None")(_.name),
        ),
      ),
      browser.signal(Codeswitch.language).map(codeOf),
    )(code =>
      browser
        .storage
        .set(
          Codeswitch.language,
          Language.parse(code),
        ),
    )

  private def codeOf(language: Option[Language]): String =
    language.fold("")(_.code)

  /**
    * The models, by provider, each named as its provider names it. Choosing one
    * of another provider shows that provider's key.
    */
  private def models
    (
      browser: Browser,
      chosen: Signal[LlmModel],
    )
    (using Granted[Storage])
    : HtmlElement = chooser(
    browser,
    "model",
    Translator
      .models
      .map((provider, models) =>
        optGroup(
          labelAttr := provider.displayName,
          models.map(model => option(value := model.name, caption(model))),
        ),
      ),
    chosen.map(_.name),
  )(name =>
    IO.fromOption(LlmModel.parse(name))(Problem(s"No model is named $name."))
      .flatMap(browser.storage.set(Translator.model, _)),
  )

  private def caption(model: LlmModel): String =
    if model == Translator.defaultModel then s"${ model.id } (quick)"
    else model.id

  /**
    * The field for the API key of the chosen model's `provider`, which keeps
    * each provider's key apart.
    */
  private def keyField
    (
      browser: Browser,
      provider: Signal[LlmProvider],
    )
    (using Granted[Storage])
    : HtmlElement = input(
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
      change[(String, LlmProvider)](browser)((key, provider) =>
        browser
          .storage
          .update(Codeswitch.apiKeys)(_.updated(provider.key, key.trim))
          .void,
      ),
  )

  private def keyName(provider: LlmProvider): String =
    s"${ provider.displayName } API key"

  /**
    * The menu `id` of `options`, showing the `chosen` one, which gives `choose`
    * each choice.
    */
  private def chooser
    (
      browser: Browser,
      id: String,
      options: Seq[HtmlElement],
      chosen: Signal[String],
    )
    (choose: String => IO[Unit])
    (using Granted[Storage])
    : HtmlElement = select(
    idAttr := id,
    options,
    value <-- chosen,
    onChange.mapToValue --> change(browser)(choose),
  )

  /**
    * Runs `program` with each change, then clears the problem shown, which the
    * change may have solved.
    */
  private def change[A]
    (browser: Browser)
    (program: A => IO[Unit])
    (using Granted[Storage])
    : Observer[A] =
    browser.observer(value => program(value) *> dismiss(browser))

  /**
    * Clears the problem shown, and the mark on the toolbar button, both of
    * every tab and of the tab that the popup was opened over.
    */
  private def dismiss(browser: Browser)(using Granted[Storage]): IO[Unit] =
    browser.storage.set(Codeswitch.problem, "") *>
      Codeswitch.mark(browser.action, None, "") *>
      browser
        .tabs
        .active
        .flatMap(
          _.traverse_(tab => Codeswitch.mark(browser.action, Some(tab.id), "")),
        )
