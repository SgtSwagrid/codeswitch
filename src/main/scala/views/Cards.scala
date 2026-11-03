package com.alecdorrington.codeswitch.views

import cats.effect.IO
import com.alecdorrington.annotatedtextformat.dom.Tooltip
import com.alecdorrington.codeswitch.models.{
  voiceFor, Answer, Card, Example, Familiarity, Headword, Lookup, Rating,
  Recording, Row, Sense, Source, Standing, Table,
}
import com.alecdorrington.omnibrowser.megalingual.{Speech, Voice}
import org.scalajs.dom.*
import scala.collection.mutable
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/**
  * Makes the card that the tooltip of a hint or a translation shows of its word
  * (see [[models.Headword]]): at once, its dictionary form, a button that says
  * it aloud, where the browser has a voice for its language, its word class,
  * its meaning in its place, the original of a gloss, or the word that a
  * translation stands for, and a button for each [[models.Answer]] to whether
  * the reader knew it, beneath how the word stands with the reader, as `stand`
  * finds it, which follows their answer; then, once the tooltip has stayed open
  * a moment, what the dictionary has of the word, as `look` finds it: how it is
  * said, a speaker's recording of it, if any, which the button then plays,
  * shown then if the browser has no voice, its first senses, and the one in
  * which the reader met it marked, and where its entry and recording are from;
  * and, while the reader asks for more, all of its senses, with their examples,
  * the tables of its forms, the one in which the reader met it marked, and
  * where it came from. Each card opens as the reader left the last, with more
  * or with less.
  *
  * @param rate
  *   What keeps each answer that the reader gives.
  * @param edit
  *   What opens the editor of a lexeme of the reader's lexicon, by its own.
  * @param look
  *   What looks a word up, giving its card, if it has one, or why it could not
  *   be had.
  * @param stand
  *   What finds how a word stands with the reader, if it can be had.
  * @param hear
  *   What fetches the recording at an address, giving its bytes in Base64.
  * @param speech
  *   The browser's speech, which says words aloud.
  * @param run
  *   What runs a program at once, without waiting for it.
  */
private final class Cards
  (
    rate: Rating => Unit,
    edit: String => Unit,
    look: Lookup => IO[Either[String, Option[Card]]],
    stand: Headword => IO[Option[Standing]],
    hear: String => IO[String],
    speech: Speech,
    run: IO[Unit] => Unit,
  ):

  /**
    * The cards looked up, by what was looked up, until forgotten, each lookup
    * keeping what it finds in those there were as it began.
    */
  private var found = mutable.HashMap.empty[Lookup, Option[Card]]

  /**
    * Forgets the cards looked up, as for a reader who has changed their
    * languages, so that no lookup made before keeps its card for them.
    */
  def forget(): Unit = found = mutable.HashMap.empty

  /** Whether the reader last asked for more of a card. */
  private var detailed = false

  /**
    * The card of `headword`, which fills in with what the dictionary has of it
    * while `showing` lasts, at once if it was looked up before.
    */
  def apply
    (
      headword: Headword,
      showing: Tooltip.Showing,
    )
    : Node =
    val slots = Cards.slots(
      headword,
      rate,
      Speaker(headword, hear, speech, run),
      detailed,
    )
    run(
      stand(headword).flatMap(found => IO(found.foreach(slots.standing.show))),
    )
    run(voicing(headword, slots.speaker))
    Lookup
      .checked(Cards.lookup(headword))
      .foreach(lookup => run(filling(lookup, headword.form, showing, slots)))
    slots.card

  /**
    * Gives `speaker` the browser's voice for the language of `headword`, once
    * the browser has listed its voices, if it has one (see
    * [[models.voiceFor]]).
    */
  private def voicing(headword: Headword, speaker: Speaker): IO[Unit] = speech
    .voices
    .map(voiceFor(headword.language, _))
    .flatMap(found => IO(found.foreach(speaker.voiced)))

  /**
    * Fills `slots` with what the dictionary has of `lookup`, met as `seen`: at
    * once, if it was looked up before, or else once `showing` has lasted
    * [[Cards.patience]], so that a pointer passing over hints looks nothing up.
    */
  private def filling
    (
      lookup: Lookup,
      seen: String,
      showing: Tooltip.Showing,
      slots: Slots,
    )
    : IO[Unit] = IO(found).flatMap(kept =>
    kept
      .get(lookup)
      .fold(
        IO.sleep(Cards.patience) *> IO(showing.isCurrent).ifM(
          IO(Cards.status(slots.entry, "Looking up…")) *>
            look(lookup).flatMap(outcome =>
              IO(outcome.foreach(kept.update(lookup, _))) *>
                IO(fill(slots, seen, outcome)),
            ),
          IO.unit,
        ),
      )(card => IO(fill(slots, seen, Right(card)))),
  )

  /**
    * Fills `slots` with `card`, of a word met as `seen`, or says that the
    * dictionary has none, or why it could not be had.
    */
  private def fill
    (
      slots: Slots,
      seen: String,
      outcome: Either[String, Option[Card]],
    )
    : Unit = outcome match
    case Right(Some(card)) =>
      Cards.heading(
        slots.head,
        card.entry.word,
        slots.speaker.element,
        card.entry.wordClass,
        card.entry.genders,
        card.entry.ipa,
      )
      card.recording.foreach(slots.speaker.recorded)
      Cards.clear(slots.entry)
      Cards.contents(card, seen).foreach(slots.entry.appendChild)
      card
        .lexeme
        .foreach(id =>
          slots.foot.appendChild(Cards.editor(card.entry.word, () => edit(id))),
        )
      if Cards.more(card) then slots.foot.appendChild(toggle(slots.card))
      slots.foot.appendChild(Cards.source(card))
    case Right(None)   => Cards.status(slots.entry, "No dictionary entry.")
    case Left(problem) => Cards.status(slots.entry, problem)

  /**
    * The button that shows more of `card`, or less, which each card after opens
    * as it was left.
    */
  private def toggle(card: Element): Element =
    val button = Cards.element("button", "more")
    button.setAttribute("type", "button")
    def show(): Unit =
      card.classList.toggle("detailed", detailed)
      button.setAttribute("aria-expanded", detailed.toString)
      button.textContent = if detailed then "Less" else "More"
    show()
    button.addEventListener(
      "click",
      (event: Event) =>
        if event.isTrusted then
          detailed = !detailed
          show(),
    )
    button

/**
  * The parts of a card that fill in once its word is looked up: the `card`
  * itself, its `head`, the button in it that says the word, `speaker`, the
  * `entry` within it, how its word stands with the reader, `standing`, and its
  * `foot`, which holds that line and the buttons, and then the button that
  * shows more and where the entry is from.
  */
private final case class Slots
  (
    card: Element,
    head: Element,
    speaker: Speaker,
    entry: Element,
    standing: StandingLine,
    foot: Element,
  )

/**
  * The button of a card that says its word, of `headword`, aloud, shown once it
  * can: a speaker's recording of it, once the card has one, which `hear`
  * fetches, and `run` plays, or else, or if that cannot be played, the
  * browser's voice for its language, through `speech`, once the browser has
  * one.
  */
private final class Speaker
  (
    headword: Headword,
    hear: String => IO[String],
    speech: Speech,
    run: IO[Unit] => Unit,
  ):

  /** Where the button goes in the card's head, which holds it once shown. */
  val element: Element = Cards.element("span", "say")

  private var recording = Option.empty[Recording]

  private var voice = Option.empty[Voice]

  /** Shows the button, which then says the word in `found`, a voice for it. */
  def voiced(found: Voice): Unit =
    voice = Some(found)
    show()

  /** Shows the button, which then plays `found`, a recording of the word. */
  def recorded(found: Recording): Unit =
    recording = Some(found)
    show()

  private def show(): Unit = if element.firstChild == null then
    element.appendChild(Cards.speaker(headword, () => say()))

  /**
    * Says the word: plays its recording, if it has one (see [[Recordings]]), or
    * else says it in the browser's voice, as it does if the recording cannot be
    * played.
    */
  private def say(): Unit = recording.fold(speak())(found =>
    run(Recordings.play(hear(found.url)).handleError(_ => speak())),
  )

  /**
    * Says the word in the browser's voice for it, if it has one, stopping any
    * recording, and warning only if it cannot be said.
    */
  private def speak(): Unit =
    Recordings.stop()
    voice.foreach(found =>
      run(
        speech
          .say(headword.word, found)
          .handleErrorWith(failure =>
            IO(console.warn(s"Codeswitch: ${ failure.getMessage }")),
          ),
      ),
    )

/**
  * The line of a card that says how its word stands with the reader, once it is
  * known, and again as the reader rates the word.
  */
private final class StandingLine:

  val element: Element = Cards.element("div", "standing")

  private var standing = Option.empty[Standing]

  /** Says that the word stands as `found`. */
  def show(found: Standing): Unit =
    standing = Some(found)
    Cards.standing(element, found)

  /** Says how the word stands once the reader has given `answer` for it. */
  def rated(answer: Answer): Unit = standing
    .map(Cards.after(_, answer))
    .foreach(show)

private object Cards:

  /**
    * How long a card's tooltip stays open before its word is looked up, as by a
    * pointer that has stopped on it, rather than passing over it.
    */
  val patience: FiniteDuration = 300.millis

  /**
    * How tall a card's tooltip keeps room for on the side of its word that it
    * opens on, in pixels: a little taller than a card filled in from its entry,
    * which first shows without it, so that, kept to that side as the pointer
    * crosses to it, it seldom scrolls.
    */
  val reserve: Double = 340

  /** How many senses a card shows until the reader asks for more. */
  private val first = 3

  /** How many examples of each sense a card shows. */
  private val examples = 2

  /** The class of the row of buttons in a card. */
  private val answering = "answers"

  /** The class of the button last pressed in a card. */
  private val chosen = "chosen"

  /**
    * How a card looks, within its tooltip's shadow root, each rule for its host
    * `!important`, so that the page's own rules for every element do not
    * restyle it: in three parts, each of its own colour, its top, in teal,
    * which names the word, its entry, whose tables are as wide as it and
    * faintly tinted, their headings more, and its foot, where the reader
    * answers, beneath how the word stands, in [[Scores]]' own look, its rows
    * centred, so that More stays under the pointer as the card, centred on its
    * word, widens; all dark where the reader's system is. What is shown only
    * while the reader asks for more is hidden otherwise, and holds nothing that
    * takes the focus, which a hidden part could not; and while they ask for
    * more, the card is as wide as its widest table, up to a limit, as the rest
    * of it, in `contain: inline-size`, widens it no more.
    */
  val styles =
    s""":host { --text: #1f2937 !important; --muted: #4b5563 !important; --surface: #fff !important; --head: #0f766e !important; --foot: #f0fdfa !important; --rule: #e5e7eb !important; --foot-rule: #cdeae5 !important; --accent: #0f766e !important; --mark: #ccfbf1 !important; --on-mark: #134e4a !important; --button-rule: #99d6cd !important; --button-hover: #ccfbf1 !important; --table: #f6faf9 !important; --table-head: #e6f2f0 !important; --table-rule: #dbe9e6 !important; --table-head-text: #115e59 !important; border-radius: 10px !important; background: var(--surface) !important; color: var(--text) !important; font: 400 14px/1.5 system-ui, sans-serif !important; box-shadow: 0 8px 24px rgba(0, 0, 0, 0.18), 0 1px 4px rgba(0, 0, 0, 0.12) !important; }
       |@media (prefers-color-scheme: dark) { :host { --text: #e5e7eb !important; --muted: #9ca3af !important; --surface: #1f2937 !important; --head: #115e59 !important; --foot: #111827 !important; --rule: #374151 !important; --foot-rule: #374151 !important; --accent: #2dd4bf !important; --mark: #134e4a !important; --on-mark: #ccfbf1 !important; --button-rule: #4b5563 !important; --button-hover: #134e4a !important; --table: #242e3c !important; --table-head: #2b3645 !important; --table-rule: #364152 !important; --table-head-text: #cbd5e1 !important; } }
       |.card { display: flex; flex-direction: column; max-width: 380px; overflow-wrap: break-word; }
       |.card.detailed { min-width: min(380px, calc(100vw - 16px)); max-width: 720px; }
       |.card.detailed :is(.top, .foot, .senses, .etymology, .status) { contain: inline-size; }
       |.top { padding: 10px 14px; background: var(--head); color: #fff; }
       |.head { display: flex; flex-wrap: wrap; align-items: baseline; column-gap: 8px; row-gap: 2px; }
       |.word { font-size: 18px; font-weight: 700; line-height: 1.25; }
       |.say { display: contents; }
       |.word-class { font-style: italic; }
       |.word-class, .ipa { font-size: 13px; color: rgba(255, 255, 255, 0.9); }
       |.tip { margin-top: 4px; font-weight: 600; }
       |.entry { padding: 10px 14px 12px; }
       |.entry:empty { display: none; }
       |.status, .example .translation { color: var(--muted); }
       |.senses { margin: 0; padding: 0 0 0 20px; }
       |.senses li { padding-left: 2px; }
       |.senses li + li { margin-top: 4px; }
       |.senses li::marker { color: var(--muted); }
       |mark { margin: 0 -4px; padding: 1px 4px; border-radius: 4px; background: var(--mark); color: var(--on-mark); -webkit-box-decoration-break: clone; box-decoration-break: clone; }
       |.example { margin-top: 2px; font-size: 13px; font-style: italic; }
       |.example .translation { font-style: normal; }
       |.details { display: flex; flex-direction: column; gap: 12px; margin-top: 12px; padding-top: 12px; border-top: 1px solid var(--rule); }
       |.card:not(.detailed) :is(.extra, .example, .details) { display: none; }
       |.scroll { overflow-x: auto; border-radius: 6px; scrollbar-width: thin; scrollbar-color: var(--muted) transparent; }
       |table { width: 100%; border-collapse: separate; border-spacing: 0; font-size: 13px; }
       |caption { padding-bottom: 4px; text-align: left; font-size: 12px; font-weight: 600; color: var(--accent); }
       |th, td { padding: 4px 10px; text-align: left; white-space: nowrap; background: var(--table); border-top: 1px solid var(--table-rule); }
       |th, thead td { background: var(--table-head); color: var(--table-head-text); font-weight: 600; }
       |tbody th { width: 1%; }
       |caption + * > tr:first-child > * { border-top: 0; }
       |caption + * > tr:first-child > :first-child { border-top-left-radius: 6px; }
       |caption + * > tr:first-child > :last-child { border-top-right-radius: 6px; }
       |tbody > tr:last-child > :first-child { border-bottom-left-radius: 6px; }
       |tbody > tr:last-child > :last-child { border-bottom-right-radius: 6px; }
       |.etymology { font-size: 13px; }
       |a { color: inherit; text-decoration: underline; }
       |.foot { display: flex; flex-wrap: wrap; justify-content: center; align-items: center; gap: 10px; padding: 10px 14px; background: var(--foot); border-top: 1px solid var(--foot-rule); }
       |.standing { display: grid; flex: 1 0 100%; grid-template-columns: repeat(3, minmax(0, 1fr)); column-gap: 16px; }
       |.standing:empty { display: none; }
       |.$answering { display: flex; flex: 1 0 100%; justify-content: center; align-items: center; gap: 5px; }
       |.$answering .end { font-size: 12px; color: var(--muted); }
       |.$answering .end:first-child { margin-right: 3px; }
       |.$answering .end:last-child { margin-left: 3px; }
       |button { all: initial; box-sizing: border-box; min-width: 26px; padding: 3px 8px; border: 1px solid var(--button-rule); border-radius: 6px; background: var(--surface); color: var(--accent); font: 600 12px/1.4 system-ui, sans-serif; text-align: center; cursor: pointer; }
       |button:hover { background: var(--button-hover); }
       |button.$chosen { border-color: var(--accent); background: var(--accent); color: var(--surface); }
       |:focus-visible { outline: 2px solid var(--accent); outline-offset: 2px; }
       |.top :focus-visible { outline-color: #fff; }
       |.source { font-size: 12px; color: var(--muted); text-align: center; }
       |.edit { border-color: transparent; background: transparent; text-decoration: underline; }
       |.speak { display: inline-flex; align-self: center; min-width: 0; padding: 2px; border-color: transparent; background: transparent; color: #fff; }
       |.speak:hover { background: rgba(255, 255, 255, 0.18); }
       |.speak svg { width: 16px; height: 16px; }
       |""".stripMargin + Scores.styles

  /**
    * The parts of the card of `headword`, as it shows at once: its dictionary
    * form, the `speaker` that says it, shown once the browser has a voice for
    * it, its word class, its tip, but where that is the word, and its buttons,
    * which give `rate` each answer, with more shown if `detailed`.
    */
  def slots
    (
      headword: Headword,
      rate: Rating => Unit,
      speaker: Speaker,
      detailed: Boolean,
    )
    : Slots =
    val slots = Slots(
      element("div", "card"),
      element("div", "head"),
      speaker,
      element("div", "entry"),
      StandingLine(),
      element("div", "foot"),
    )
    slots.card.classList.toggle("detailed", detailed)
    heading(
      slots.head,
      headword.word,
      speaker.element,
      headword.wordClass,
      Nil,
      None,
    )
    List(
      slots.standing.element,
      answers(
        headword,
        rating =>
          rate(rating)
          slots.standing.rated(rating.answer),
      ),
    ).foreach(slots.foot.appendChild)
    List(
      holding(
        "div",
        "top",
        slots.head ::
          Option
            .when(headword.tip.trim != headword.word.trim)(
              text("div", "tip", headword.tip),
            )
            .toList,
      ),
      slots.entry,
      slots.foot,
    ).foreach(slots.card.appendChild)
    slots

  /**
    * Heads a card in `head` with `word`, `say`, which holds the button that
    * says it, its word class, `wordClass`, and its `genders`, if known, and how
    * it is said, `ipa`, if known.
    */
  def heading
    (
      head: Element,
      word: String,
      say: Element,
      wordClass: String,
      genders: List[String],
      ipa: Option[String],
    )
    : Unit =
    clear(head)
    head.appendChild(text("span", "word", word))
    head.appendChild(say)
    List(
      Some(grammar(wordClass, genders))
        .filter(_.nonEmpty)
        .map("word-class" -> _),
      ipa.map("ipa" -> _),
    ).flatten
      .foreach((kind, content) => head.appendChild(text("span", kind, content)))

  /**
    * A button that has `say` say `headword` aloud, but never for a click that a
    * page script makes.
    */
  def speaker(headword: Headword, say: () => Unit): Element =
    val button = element("button", "speak")
    button.setAttribute("type", "button")
    button.setAttribute(
      "aria-label",
      s"Say ${ headword.word } aloud",
    )
    button.setAttribute("title", "Say it aloud")
    button.appendChild(loudspeaker)
    button.addEventListener(
      "click",
      (event: Event) => if event.isTrusted then say(),
    )
    button

  /**
    * A button that has `edit` open the editor of the entry for `word`, in the
    * reader's lexicon, but never for a click that a page script makes.
    */
  def editor(word: String, edit: () => Unit): Element =
    val button = text("button", "edit", "Edit")
    button.setAttribute("type", "button")
    button.setAttribute(
      "aria-label",
      s"Edit the entry for $word",
    )
    button.setAttribute(
      "title",
      "Edit this entry in your lexicon",
    )
    button.addEventListener(
      "click",
      (event: Event) => if event.isTrusted then edit(),
    )
    button

  /** A loudspeaker, drawn in the colour of the text around it. */
  private def loudspeaker: Element =
    val icon = drawn("svg")
    icon.setAttribute("viewBox", "0 0 24 24")
    icon.setAttribute("aria-hidden", "true")
    List(
      "M3 9h4l5-4v14l-5-4H3z"        -> "currentColor",
      "M15.5 8.5a5 5 0 0 1 0 7"      -> "none",
      "M18.5 5.5a9.2 9.2 0 0 1 0 13" -> "none",
    ).foreach((shape, fill) =>
      val path = drawn("path")
      path.setAttribute("d", shape)
      path.setAttribute("fill", fill)
      path.setAttribute("stroke", "currentColor")
      path.setAttribute("stroke-width", "2")
      path.setAttribute("stroke-linecap", "round")
      path.setAttribute("stroke-linejoin", "round")
      icon.appendChild(path),
    )
    icon

  /** An SVG element of `tag`. */
  private def drawn(tag: String): Element =
    document.createElementNS("http://www.w3.org/2000/svg", tag)

  /**
    * What the card of `headword` is called for a reader of the screen: its
    * dictionary form, and its word class, if known.
    */
  def label(headword: Headword): String =
    if headword.wordClass.isEmpty then headword.word
    else s"${ headword.word }, ${ inFull(headword.wordClass) }"

  /**
    * What to look `headword` up as: its dictionary form and word class, in the
    * sense of its meaning, cut to [[models.Lookup.longest]], as it is only
    * weighed against the dictionary's senses.
    */
  def lookup(headword: Headword): Lookup = Lookup(
    headword.word,
    headword.wordClass,
    headword.meaning.take(Lookup.longest),
    headword.language,
  )

  /**
    * Says in `line` how a word stands with the reader, as `standing` has it,
    * each measure as the Vocabulary tab shows it, but named (see [[Scores]]).
    */
  def standing(line: Element, standing: Standing): Unit =
    clear(line)
    Scores.all(standing).foreach(line.appendChild)

  /**
    * How a word stands once the reader, who knew it as `standing` says, gives
    * `answer` for it.
    */
  def after(standing: Standing, answer: Answer): Standing =
    standing.copy(familiarity = Familiarity.after(standing.familiarity, answer))

  /** A word class, `wordClass`, as Kaikki names it, in full. */
  def inFull(wordClass: String): String = wordClass match
    case "adj"  => "adjective"
    case "adv"  => "adverb"
    case "prep" => "preposition"
    case "pron" => "pronoun"
    case "det"  => "determiner"
    case "conj" => "conjunction"
    case "num"  => "numeral"
    case "intj" => "interjection"
    case other  => other

  /**
    * A word's class, `wordClass`, in full, and its `genders`, as a card heads
    * it, as `noun, feminine`, or `noun, masculine or feminine` for a word that
    * may take either.
    */
  def grammar(wordClass: String, genders: List[String]): String = List(
    inFull(wordClass),
    genders.mkString(" or "),
  ).filter(_.nonEmpty).mkString(", ")

  /**
    * What a card shows of `card`, of a word met as `seen`, before its buttons,
    * in order: the word's senses, and what is shown only while the reader asks
    * for more.
    */
  def contents(card: Card, seen: String): List[Element] =
    val more = details(card, seen)
    List(
      Option.when(card.entry.senses.nonEmpty)(
        senses(card.entry.senses, card.fitting),
      ),
      Option.when(more.nonEmpty)(holding("div", "details", more)),
    ).flatten

  /** Whether there is more of `card` to show than a card shows at first. */
  def more(card: Card): Boolean =
    val senses = card.entry.senses
    senses.indices.exists(!shown(card.fitting)(_)) ||
    senses.exists(_.examples.nonEmpty) || card.tables.nonEmpty ||
    card.entry.etymology.nonEmpty

  /**
    * Whether a card shows the sense at `index` before the reader asks for more:
    * if it is among the [[first]], or it is the one `fitting` the meaning.
    */
  private def shown(fitting: Option[Int])(index: Int): Boolean =
    index < first || fitting.contains(index)

  /**
    * The senses of a word, each with its examples, numbered as in its entry,
    * the gloss of the one `fitting` its meaning, if any, marked, and those not
    * [[shown]], and the examples, shown only while the reader asks for more.
    */
  private def senses
    (
      senses: List[Sense],
      fitting: Option[Int],
    )
    : Element = holding(
    "ol",
    "senses",
    senses
      .zipWithIndex
      .map((sense, index) =>
        attributed(
          holding(
            "li",
            if shown(fitting)(index) then "" else "extra",
            text(
              if fitting.contains(index) then "mark" else "span",
              "gloss",
              sense.glosses.mkString("; "),
            ) :: sense.examples.take(examples).map(example),
          ),
          "value",
          (index + 1).toString,
        ),
      ),
  )

  /** An example of a word's use, with its translation, if it has one. */
  private def example(example: Example): Element = holding(
    "div",
    "example",
    text("span", "text", example.text) ::
      example
        .translation
        .filter(_.nonEmpty)
        .map(translation =>
          text(
            "span",
            "translation",
            s" — $translation",
          ),
        )
        .toList,
  )

  /**
    * What a card shows of `card`, of a word met as `seen`, only while the
    * reader asks for more.
    */
  private def details(card: Card, seen: String): List[Element] = card
    .tables
    .map(table(_, seen)) ++
    card.entry.etymology.map(text("div", "etymology", _))

  /**
    * A table of a word's forms, met as `seen`, which scrolls within the card if
    * it is wider, as a verb's of many tenses is, its columns named if it has
    * any.
    */
  private def table(table: Table, seen: String): Element = holding(
    "div",
    "scroll",
    List(holding(
      "table",
      "",
      List(
        Some(text("caption", "", table.title)),
        Option.when(table.columns.nonEmpty)(holding(
          "thead",
          "",
          List(columns(table.columns)),
        )),
        Some(holding(
          "tbody",
          "",
          table.rows.map(row(_, seen)),
        )),
      ).flatten,
    )),
  )

  /** The row of a table that names its `columns`, each heading its own. */
  private def columns(columns: List[String]): Element = holding(
    "tr",
    "",
    element("td", "") ::
      columns.map(column => attributed(text("th", "", column), "scope", "col")),
  )

  /**
    * A row of a table, its label heading its cells, each that [[holds]] the
    * form `seen` marked.
    */
  private def row(row: Row, seen: String): Element = holding(
    "tr",
    "",
    attributed(text("th", "", row.label), "scope", "row") ::
      row
        .cells
        .map(cell =>
          if holds(cell, seen) then
            holding("td", "", List(text("mark", "", cell)))
          else text("td", "", cell),
        ),
  )

  /**
    * Whether `cell`, of a table of a word's forms, holds `seen`, the form in
    * which the reader met the word, as the page writes it, in any case: one of
    * the cell's forms is the whole, its parts joined (`ruft an` of
    * `ruft … an`), or a word of it (`Bänke` of `die Bänke`).
    */
  def holds(cell: String, seen: String): Boolean =
    val whole = normal(seen)
    val forms = (whole :: whole.split(' ').toList).filter(_.nonEmpty).toSet
    cell.split(",").map(normal).exists(forms)

  /** `text` in lower case, its words, and a split form's parts, one apart. */
  private def normal(text: String): String = text
    .replace('…', ' ')
    .trim
    .split("\\s+")
    .mkString(" ")
    .toLowerCase

  /** `element`, its `attribute` set to `value`. */
  private def attributed
    (
      element: Element,
      attribute: String,
      value: String,
    )
    : Element =
    element.setAttribute(attribute, value)
    element

  /**
    * Where the entry of `card` is from: Wiktionary, under its licence, put into
    * the reader's language by their model if it was, and where its recording is
    * from, if it has one, whose page gives its speaker and licence; or else
    * their model.
    */
  def source(card: Card): Element = (card.entry.source, card.page) match
    case (Source.Wiktionary(_, translated, changed), Some(page)) => holding(
        "div",
        "source",
        List(
          text("span", "", "From "),
          link("Wiktionary", page),
          text(
            "span",
            "",
            List(
              Some(", CC BY-SA"),
              Option.when(translated)(", translated by your model"),
              Option.when(changed)(", with changes"),
            ).flatten.mkString,
          ),
        ) ++
          card
            .recording
            .toList
            .flatMap(recording =>
              List(
                text("span", "", "; recording from "),
                link("Wikimedia Commons", recording.page),
              ),
            ),
      )
    case _ => text(
        "div",
        "source",
        "Written by your model, so it may have mistakes.",
      )

  /** A link to `page`, named `name`, which opens in a tab of its own. */
  private def link(name: String, page: String): Element =
    val link = text("a", "", name)
    link.setAttribute("href", page)
    link.setAttribute("target", "_blank")
    link.setAttribute("rel", "noopener noreferrer")
    link

  /**
    * A button for each answer to how well the reader knew `headword`, on a
    * scale of five, between what its ends mean, each button named by its point
    * and what it means.
    */
  def answers(headword: Headword, rate: Rating => Unit): Element =
    val row = element("div", answering)
    row.setAttribute("role", "group")
    row.setAttribute(
      "aria-label",
      s"How well did you know ${ headword.word }?",
    )
    row.appendChild(end(Answer.values.head))
    Answer
      .values
      .foreach(answer =>
        row.appendChild(button(
          row,
          answer,
          () =>
            rate(Rating(
              headword.word,
              headword.language,
              headword.level,
              answer,
            )),
        )),
      )
    row.appendChild(end(Answer.values.last))
    row

  /**
    * What `answer`, at an end of the scale, means, beside it, which the
    * buttons' names already say to a reader of the screen.
    */
  private def end(answer: Answer): Element =
    val end = text("span", "end", answer.label)
    end.setAttribute("aria-hidden", "true")
    end

  /**
    * A button in `row` for `answer`, showing its point, named by it and what it
    * means, which gives it with `give`, and shows that it was pressed, the
    * first time the reader presses one in the row, but never for a click that a
    * page script makes.
    */
  private def button
    (
      row: Element,
      answer: Answer,
      give: () => Unit,
    )
    : Element =
    val button = element("button", "")
    button.setAttribute("type", "button")
    button.setAttribute("aria-pressed", "false")
    button.setAttribute(
      "aria-label",
      s"${ answer.point }, ${ answer.label }",
    )
    button.setAttribute("title", answer.label)
    button.textContent = answer.point.toString
    button.addEventListener(
      "click",
      (event: Event) =>
        if event.isTrusted && !answered(row) then
          give()
          press(row, button),
    )
    button

  /** Whether a button in `row` has been pressed. */
  private def answered(row: Element): Boolean =
    row.querySelector("[aria-pressed='true']") != null

  /** Shows that `button` was pressed, of those in `row`. */
  private def press(row: Element, button: Element): Unit = row
    .querySelectorAll("button")
    .foreach(other =>
      other.classList.toggle(chosen, other == button)
      other.setAttribute(
        "aria-pressed",
        (other == button).toString,
      ),
    )

  /** Says `status` in `entry`, in place of anything there. */
  def status(entry: Element, status: String): Unit =
    clear(entry)
    entry.appendChild(text("div", "status", status))

  /** Takes everything out of `parent`. */
  def clear(parent: Element): Unit =
    while parent.firstChild != null do parent.removeChild(parent.firstChild)

  /** An element of `tag`, of the class `kind`, if any, holding `children`. */
  private def holding
    (
      tag: String,
      kind: String,
      children: List[Element],
    )
    : Element =
    val parent = element(tag, kind)
    children.foreach(parent.appendChild)
    parent

  /** An element of `tag`, of the class `kind`, if any, holding `content`. */
  def text(tag: String, kind: String, content: String): Element =
    val element = this.element(tag, kind)
    element.textContent = content
    element

  /** An element of `tag`, of the class `kind`, if any. */
  def element(tag: String, kind: String): Element =
    val element = document.createElement(tag)
    if kind.nonEmpty then element.classList.add(kind)
    element
