package com.alecdorrington.codeswitch

import cats.effect.IO
import cats.syntax.all.*
import com.alecdorrington.iris.{LlmModel, LlmProvider}
import com.alecdorrington.omnibrowser.{Browser, Granted, Page, StorageKey}
import com.alecdorrington.omnibrowser.Permission.Storage
import com.alecdorrington.omnibrowser.laminar.*
import com.raquo.laminar.api.L.*
import scala.concurrent.duration.DurationInt

object Popup:

  /** The popup, which Laminar renders, styled by `popup.css`. */
  def page(using Granted[Storage]): Page = LaminarPage(Popup(_).view)
    .withStylesheets("popup.css")

  /**
    * The least time for which the popup shows a change being saved, as saving
    * one is too quick to see.
    */
  private val spin = 500.millis

/**
  * The popup of the toolbar button, in which the reader chooses the language to
  * learn, how much of each page to translate and the model to translate with,
  * and gives an API key for its provider. Each change is kept in `browser`'s
  * storage at once, as the foot of the popup shows, and clears the problem
  * shown.
  */
private final class Popup(browser: Browser)(using Granted[Storage]):

  private val storage  = browser.storage
  private val model    = browser.signal(Translator.model)
  private val provider = model.map(_.provider)

  /** How many changes are still being saved. */
  private val pending = Var(0)

  /** How saving the last change to finish went. */
  private val last = Var(Status.Idle)

  /** What the foot of the popup says: that changes are saving, while any are. */
  private val status = pending
    .signal
    .combineWith(last.signal)
    .map((pending, last) => if pending > 0 then Status.Saving else last)

  def view: HtmlElement = div(
    headerTag(
      img(src := "icons/icon-32.png", alt := ""),
      h1(Codeswitch.name),
    ),
    label(span("Language to learn"), languages),
    intensity,
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
    p(
      cls  := "status",
      role := "status",
      cls <-- status.map(_.toString.toLowerCase),
      child.text <-- status.map(_.text),
    ),
    // Each change, as its input event bubbles up, may solve the problem shown,
    // except the slider's, which stops each step's at itself.
    onInput --> browser.observer(_ => dismiss),
  )

  private def languages = select(
    idAttr := "language",
    // Its text first, as Laminar would not set the value `""` that an option
    // without text already reads, and the option would then read `None`.
    option("None", value := ""),
    Language
      .values
      .map(language => option(value := language.code, language.name)),
    value <-- browser.signal(Codeswitch.language).map(_.fold("")(_.code)),
    onChange.mapToValue.map(Language.parse) --> saver(Codeswitch.language),
  )

  /**
    * The share of the words to translate, on a slider of whole percentages,
    * shown beside its name as it moves. It is kept, and clears the problem
    * shown, at once when it is let go or a key moves it a step, as the popup
    * may close at any moment; but a key held down, repeating, keeps only where
    * it is let go, as sync storage allows 120 writes a minute. While the slider
    * has focus, the value kept does not move it, lest a step kept late move it
    * back.
    */
  private def intensity =
    val shown   = Var(percentOf(Codeswitch.intensity.default))
    val text    = shown.signal.map(percent => s"$percent% of words")
    val focused = Var(false)
    val held    = Var(false)
    val keep    = Observer.combine(
      saver(Codeswitch.intensity).contramap[Int](_ / 100.0),
      browser.observer[Int](_ => dismiss),
    )
    label(
      cls := "slider",
      span(
        span(idAttr := "intensity-name", "Intensity"),
        // Read out as the slider's value, rather than as a part of its name.
        outputTag(aria.hidden := true, child.text <-- text),
      ),
      input(
        idAttr          := "intensity",
        typ             := "range",
        minAttr         := "0",
        maxAttr         := percentOf(Words.maxIntensity).toString,
        stepAttr        := "1",
        aria.labelledBy := "intensity-name",
        aria.valueText <-- text,
        value <-- shown.signal.map(_.toString),
        browser.signal(Codeswitch.intensity).map(percentOf) -->
          Observer[Int](percent => if !focused.now() then shown.set(percent)),
        onFocus.mapTo(true) --> focused,
        onBlur.mapTo(false) --> focused,
        onInput.stopPropagation.mapToValue.map(_.toInt) --> shown,
        onChange.mapToValue.map(_.toInt).filter(_ => !held.now()) --> keep,
        onKeyDown.filter(_.repeat).mapTo(true) --> held,
        // Before `held` is cleared, as the listeners run in this order.
        onKeyUp.mapToValue.map(_.toInt).filter(_ => held.now()) --> keep,
        onKeyUp.mapTo(false) --> held,
      ),
    )

  private def percentOf(share: Double): Int = math.round(share * 100).toInt

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
      .collect { case Some(model) => model } --> saver(Translator.model),
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
      saving[(String, LlmProvider)]((key, provider) =>
        storage
          .update(Codeswitch.apiKeys)(_.updated(provider.key, key.trim))
          .void,
      ),
  )

  private def keyName(provider: LlmProvider) =
    s"${ provider.displayName } API key"

  /** Gives `key` each value that the observer observes, as [[saving]] does. */
  private def saver[A](key: StorageKey[A]): Observer[A] =
    saving(storage.set(key, _))

  /**
    * Runs `save` with each value that the observer observes, showing at the
    * foot of the popup that it is saving, then whether it saved.
    */
  private def saving[A](save: A => IO[Unit]): Observer[A] =
    browser.observer(value => keep(save(value)))

  /**
    * Runs `save`, showing it saving for at least [[Popup.spin]], then how it
    * went. `save` begins in the event's own handler, as `browser.run` runs a
    * program there up to its first asynchronous step, since the popup may close
    * at once, before any later step would run; so nothing asynchronous, as
    * starting a fiber, may come before it.
    */
  private def keep(save: IO[Unit]): IO[Unit] = IO(pending.update(_ + 1)) *>
    IO.monotonic
      .flatMap(started =>
        save.attempt <*
          IO.monotonic
            .flatMap(now =>
              IO.sleep((Popup.spin - (now - started)).max(0.millis)),
            ),
      )
      .flatTap(outcome => IO(finish(outcome.isRight)))
      .rethrow

  /** Stops showing a change saving, as it has saved if `saved`. */
  private def finish(saved: Boolean): Unit =
    last.set(if saved then Status.Saved else Status.Failed)
    pending.update(_ - 1)

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

/**
  * What the foot of the popup says of the reader's changes, as [[text]], styled
  * by the class in `popup.css` that is its name in lower case.
  */
private enum Status(val text: String):

  case Idle   extends Status("")
  case Saving extends Status("Saving…")
  case Saved  extends Status("Saved")
  case Failed extends Status("Not saved")
