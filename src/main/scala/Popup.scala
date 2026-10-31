package com.alecdorrington.codeswitch

import cats.effect.IO
import cats.syntax.all.*
import com.alecdorrington.iris.{LlmModel, LlmProvider}
import com.alecdorrington.megalingual.Language
import com.alecdorrington.megalingual.Language.English
import com.alecdorrington.megalingual.laminar.LanguagePicker
import com.alecdorrington.omnibrowser.{
  Browser, Granted, Page, StorageKey, TabId,
}
import com.alecdorrington.omnibrowser.Permission.Storage
import com.alecdorrington.omnibrowser.laminar.*
import com.raquo.laminar.api.L.*
import org.scalajs.dom
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
  * The popup of the toolbar button, in which the reader chooses their own
  * language, the others they read fluently, how much of the pages in those to
  * translate, the languages they are learning and their level in each, and the
  * model to ask, and gives an API key for its provider. Each change is kept in
  * `browser`'s storage at once, as the foot of the popup shows, and clears the
  * problem shown, of the tab that the popup was opened over.
  */
private final class Popup(browser: Browser)(using Granted[Storage]):

  private val storage  = browser.storage
  private val model    = browser.signal(Translator.model)
  private val provider = model.map(_.provider)
  private val fluent   = browser.signal(Codeswitch.fluent)
  private val learning = browser.signal(Codeswitch.learning)

  /**
    * The reader's own language, as chosen, or else as it is taken to be until
    * they choose it.
    */
  private val native = browser
    .signal(Codeswitch.native)
    .combineWith(learning)
    .map((native, learning) =>
      native.fold(Learner.own(learning.map(_.language)))(learnable.resolve),
    )

  /**
    * The languages in any of the reader's three lists, their own, the others
    * they read fluently and those they are learning, none of which is offered
    * for another.
    */
  private val placed = native
    .combineWith(fluent, learning)
    .map((native, fluent, learning) =>
      native :: fluent ++ learning.map(_.language),
    )
    .distinct

  /** The tab that the popup was opened over, once it is known. */
  private val tab: EventStream[TabId] = browser
    .events(browser.tabs.active)
    .collect { case Some(tab) => tab.id }

  /**
    * Whether the picker that adds to the list of fluent languages is to take
    * the focus once it is made again, as after a language is added with it, or
    * the last taken out of the list.
    */
  private val refocusFluent = Var(false)

  /** Whether the picker that adds a language to learn is to, as above. */
  private val refocusLearning = Var(false)

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
    field(
      "native",
      "Native language",
      Val("Meanings are shown in this language."),
    )(natives),
    field(
      "fluent",
      "Other fluent languages",
      Val(
        "Pages in these, or in your native language, have some words translated into the languages you are learning. Hover over one for the original and its dictionary entry.",
      ),
    )(
      ul(
        cls := "languages",
        children <-- fluent.splitSeq(identity)(language =>
          li(div(
            cls := "language",
            named(language.key),
            removing(
              Codeswitch.fluent,
              language.key,
              refocusFluent,
            )(identity),
          )),
        ),
      ),
      adding(
        "Add a fluent language",
        Codeswitch.fluent,
        refocusFluent,
      )(identity),
    ),
    intensity,
    field(
      "learning",
      "Languages to learn",
      native
        .map(_.name(English))
        .map(native =>
          s"On pages in these, words a level above yours are highlighted, and harder ones are translated into $native. Hover over one for its dictionary entry.",
        ),
    )(
      ul(
        cls := "languages",
        children <-- learning.splitSeq(_.language)(learnt =>
          li(
            div(
              cls := "language",
              named(learnt.key),
              levels(learnt.key, learnt.map(_.level)),
              removing(
                Codeswitch.learning,
                learnt.key,
                refocusLearning,
              )(_.language),
            ),
            p(
              cls    := "note",
              idAttr := s"level-${ learnt.key.code }-note",
              child.text <-- learnt.map(described),
            ),
          ),
        ),
      ),
      adding(
        "Add a language to learn",
        Codeswitch.learning,
        refocusLearning,
      )(Learnt(_, Level.B1)),
    ),
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
      "Your key stays on this device, and is sent only to its provider, along with the text to translate or explain.",
    ),
    p(
      cls    := "problem",
      idAttr := "problem",
      child.text <-- tab.flatMapSwitch(browser.signal(Codeswitch.problem, _)),
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

  /**
    * Why pages in `learnt` have no words marked, if they have none: at C2, or
    * in a language whose words are not spaced apart.
    */
  private def unmarked(learnt: Learnt): String =
    if !learnt.language.spaced then
      " Words are not highlighted yet, as they are not separated by spaces."
    else if learnt.level == Level.C2 then
      " No words are above C2, so none are highlighted."
    else ""

  /**
    * The reader's level in `learnt`, as its description, and why its pages have
    * no words marked, if they have none.
    */
  private def described(learnt: Learnt): String =
    s"${ learnt.level.description.capitalize }.${ unmarked(learnt) }"

  /**
    * The reader's own language, as chosen, or else as it is taken to be until
    * they choose it, which they look for by its own name, of those in none of
    * their other lists. A picker offers the languages it is made with, so it is
    * made again when those lists change.
    */
  private def natives = div(
    child <--
      fluent
        .combineWith(learning)
        .map((fluent, learning) => fluent ++ learning.map(_.language))
        .distinct
        .map(others =>
          LanguagePicker(
            languages = offered(others),
            chosen = native,
            choose =
              chosen(saver(Codeswitch.native).contramap[Language](Some(_))),
            in = Val(English),
            label = Val("Native language"),
            ownFirst = true,
          ),
        ),
  )

  /** The languages that may be chosen, but for those alike any of `placed`. */
  private def offered(placed: List[Language]): List[Language] = learnable
    .languages
    .filterNot(language => placed.exists(_.alike(language)))

  /**
    * A picker called `name`, which adds the language chosen to the reader's
    * list kept in `key`, as `item` makes it, of those in none of their lists.
    * As it is made again for what it offers, it takes the focus once made, if
    * `refocus`, as after a language is added with it.
    */
  private def adding[A]
    (
      name: String,
      key: StorageKey[List[A]],
      refocus: Var[Boolean],
    )
    (item: Language => A) = div(
    child <-- placed.map(placed =>
      LanguagePicker
        .optional(
          languages = offered(placed),
          chosen = Val(None),
          choose = Observer
            .combine(
              chosen(saving[Language](language =>
                storage.update(key)(_ :+ item(language)).void,
              )),
              Observer[Language](_ => refocus.set(true)),
            )
            .contracollect[Option[Language]] { case Some(language) =>
              language
            },
          in = Val(English),
          label = Val(name),
          none = Val(name),
        )
        .amend(onMountCallback(context =>
          if refocus.now() then
            refocus.set(false)
            takeFocus(context.thisNode.ref.querySelector(".megalingual-button")),
        )),
    ),
  )

  /** Gives `element` the focus, if it is one that may take it. */
  private def takeFocus(element: dom.Element): Unit = element match
    case element: dom.HTMLElement => element.focus()
    case _                        => ()

  /** `language`'s flag and name, as the pickers show them. */
  private def named(language: Language) = span(
    cls := "name",
    img(
      cls := "megalingual-flag",
      src := language.flag.dataUri,
      alt := "",
    ),
    span(language.title(English)),
  )

  /**
    * A button that takes `language`, as `of` finds it in each item, out of the
    * reader's list kept in `key`, handing the focus on first (see [[handOn]]).
    */
  private def removing[A]
    (
      key: StorageKey[List[A]],
      language: Language,
      refocus: Var[Boolean],
    )
    (of: A => Language) = button(
    tpe        := "button",
    cls        := "remove",
    aria.label := s"Remove ${ language.title(English) }",
    "×",
    inContext(button => onClick --> (_ => handOn(button.ref, refocus))),
    onClick.mapToUnit --> chosen(saving[Unit](_ =>
      storage.update(key)(_.filterNot(of(_) == language)).void,
    )),
  )

  /**
    * Hands the focus on from `button`, about to be taken out with its language,
    * to the next language's button in its list, or else the one before, or else
    * to the list's picker, once it is made again, as `refocus` has it.
    */
  private def handOn
    (
      button: dom.Element,
      refocus: Var[Boolean],
    )
    : Unit = Option(button.closest("li"))
    .flatMap(item =>
      Option(item.nextElementSibling).orElse(Option(
        item.previousElementSibling,
      )),
    )
    .map(_.querySelector(".remove")) match
      case Some(next) => takeFocus(next)
      case None       => refocus.set(true)

  /**
    * Saves each choice a picker makes, and clears the problem shown, as a
    * change of any other control does through its input event, which a picker
    * has none of.
    */
  private def chosen[A](save: Observer[A]): Observer[A] =
    Observer.combine(save, browser.observer(_ => dismiss))

  /**
    * A setting called `name`, of its `controls`, above a `note` of what it
    * does. Not a label, as a label would pass a click on any choice of a picker
    * to the picker's button, but a group that its name names.
    */
  private def field
    (
      id: String,
      name: String,
      note: Signal[String],
    )
    (controls: Modifier[HtmlElement]*) = div(
    cls             := "field",
    role            := "group",
    aria.labelledBy := s"$id-name",
    span(idAttr := s"$id-name", name),
    controls,
    p(cls := "note", child.text <-- note),
  )

  /**
    * The reader's `level` in `language`, which they are learning, by its code
    * alone, so that the language's name has room beside it, described by the
    * note under it.
    */
  private def levels(language: Language, level: Signal[Level]) =
    val name = language.title(English)
    menu(
      s"level-${ language.code }",
      level,
      saving[Level](level =>
        storage
          .update(Codeswitch.learning)(_.map(learnt =>
            if learnt.language == language then learnt.copy(level = level)
            else learnt,
          ))
          .void,
      ),
    )(Level.values.toSeq.map(level => level -> level.toString))(_.toString)
      .amend(
        aria.label       := s"Your level in $name",
        aria.describedBy := s"level-${ language.code }-note",
      )

  /**
    * A menu named `id` of `choices`, each with its caption, of which the
    * `current` is chosen, each told apart by its `code`, and which gives `save`
    * each choice made.
    */
  private def menu[A]
    (
      id: String,
      current: Signal[A],
      save: Observer[A],
    )
    (choices: Seq[(A, String)])
    (code: A => String) = select(
    idAttr := id,
    // Its text first, as Laminar would not set the value `""` that an option
    // without text already reads, and the option would then read `None`.
    choices.map((choice, caption) => option(caption, value := code(choice))),
    value <-- current.map(code),
    onChange
      .mapToValue
      .map(chosen => choices.map(_._1).find(code(_) == chosen))
      .collect { case Some(choice) => choice } --> save,
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
        span(idAttr := "intensity-name", "Translate"),
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
    * Clears the mark on the toolbar button, both of every tab and of the tab
    * that the popup was opened over, and that tab's problem.
    */
  private def dismiss = Codeswitch.mark(browser.action, None, "") *>
    browser
      .tabs
      .active
      .flatMap(_.traverse_(tab =>
        Codeswitch.mark(browser.action, Some(tab.id), "") *> forget(tab.id),
      ))

  /**
    * Has the content script of `tab` forget its problem. Where none runs, as on
    * the browser's own pages, there is none to forget.
    */
  private def forget(tab: TabId) = browser
    .tabs
    .call(Codeswitch.dismiss, tab)(())
    .handleError(_ => ())

/**
  * What the foot of the popup says of the reader's changes, as [[text]], styled
  * by the class in `popup.css` that is its name in lower case.
  */
private enum Status(val text: String):

  case Idle   extends Status("")
  case Saving extends Status("Saving…")
  case Saved  extends Status("Saved")
  case Failed extends Status("Not saved")
