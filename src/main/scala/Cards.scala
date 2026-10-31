package com.alecdorrington.codeswitch

import cats.effect.IO
import com.alecdorrington.annotatedtextformat.dom.Tooltip
import org.scalajs.dom.*
import scala.collection.mutable
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/**
  * Makes the card that the tooltip of a hint or a translation shows of its word
  * (see [[Headword]]): at once, its dictionary form and part of speech, its
  * meaning in its place, the original of a gloss, or the word that a
  * translation stands for, and a button for each [[Answer]] to whether the
  * reader knew it; then, once the tooltip has stayed open a moment, what the
  * dictionary has of the word, as `look` finds it: how it is said, its
  * principal forms, its first senses and where its entry is from; and, while
  * the reader asks for more, all of its senses, with their examples, the tables
  * of its forms and where it came from. Each card opens as the reader left the
  * last, with more or with less.
  *
  * @param rate
  *   What keeps each answer that the reader gives.
  * @param look
  *   What looks a word up, giving its card, if it has one, or why it could not
  *   be had.
  * @param run
  *   What runs a program at once, without waiting for it.
  */
private final class Cards
  (
    rate: Rating => Unit,
    look: Lookup => IO[Either[String, Option[Card]]],
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
    val slots = Cards.slots(headword, rate, detailed)
    Lookup
      .checked(Cards.lookup(headword))
      .foreach(lookup => run(filling(lookup, showing, slots)))
    slots.card

  /**
    * Fills `slots` with what the dictionary has of `lookup`: at once, if it was
    * looked up before, or else once `showing` has lasted [[Cards.patience]], so
    * that a pointer passing over hints looks nothing up.
    */
  private def filling
    (
      lookup: Lookup,
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
                IO(fill(slots, outcome)),
            ),
          IO.unit,
        ),
      )(card => IO(fill(slots, Right(card)))),
  )

  /**
    * Fills `slots` with `card`, or says that the dictionary has none, or why it
    * could not be had.
    */
  private def fill
    (
      slots: Slots,
      outcome: Either[String, Option[Card]],
    )
    : Unit = outcome match
    case Right(Some(card)) =>
      Cards.heading(
        slots.head,
        card.entry.word,
        card.entry.pos,
        card.entry.genders,
        card.entry.ipa,
      )
      Cards.clear(slots.entry)
      Cards.contents(card).foreach(slots.entry.appendChild)
      if Cards.more(card) then slots.foot.appendChild(toggle(slots.card))
      slots.card.appendChild(Cards.source(card))
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
  * itself, its `head`, the `entry` within it and its `foot`.
  */
private final case class Slots
  (
    card: Element,
    head: Element,
    entry: Element,
    foot: Element,
  )

private object Cards:

  /**
    * How long a card's tooltip stays open before its word is looked up, as by a
    * pointer that has stopped on it, rather than passing over it.
    */
  val patience: FiniteDuration = 300.millis

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
    * restyle it. What is shown only while the reader asks for more is hidden
    * otherwise, and holds nothing that takes the focus, which a hidden part
    * could not.
    */
  val styles =
    s""":host { padding: 8px 10px !important; border-radius: 8px !important; background: #0f766e !important; color: #fff !important; font: 400 13px/1.45 system-ui, sans-serif !important; box-shadow: 0 4px 16px rgba(0, 0, 0, 0.3) !important; }
       |.card { display: flex; flex-direction: column; gap: 3px; max-width: 360px; overflow-wrap: break-word; }
       |.head { display: flex; flex-wrap: wrap; align-items: baseline; column-gap: 6px; }
       |.word { font-size: 15px; font-weight: 700; }
       |.pos { font-style: italic; }
       |.pos, .ipa, .label, .status, .source, .example .translation { color: rgba(255, 255, 255, 0.75); }
       |.tip { font-weight: 600; }
       |.forms { display: flex; flex-wrap: wrap; column-gap: 10px; }
       |.forms b { font-weight: 600; }
       |.senses { margin: 0; padding: 0 0 0 18px; }
       |.senses li { margin: 1px 0; }
       |.example { font-style: italic; margin-top: 1px; }
       |.example .translation { font-style: normal; }
       |.details { display: flex; flex-direction: column; gap: 6px; margin-top: 3px; }
       |.card:not(.detailed) :is(.extra, .example, .details) { display: none; }
       |.scroll { overflow-x: auto; scrollbar-width: thin; scrollbar-color: rgba(255, 255, 255, 0.5) transparent; }
       |table { border-collapse: collapse; font-size: 12px; }
       |caption { text-align: left; font-weight: 600; padding-bottom: 2px; }
       |th, td { padding: 1px 8px 1px 0; text-align: left; white-space: nowrap; border-top: 1px solid rgba(255, 255, 255, 0.2); }
       |th { font-weight: 600; }
       |thead th { border-top: 0; color: rgba(255, 255, 255, 0.75); }
       |.etymology { font-size: 12px; }
       |.source { font-size: 11px; }
       |a { color: inherit; text-decoration: underline; }
       |a:focus-visible { outline: 2px solid #fff; outline-offset: 1px; }
       |.foot { display: flex; flex-wrap: wrap; align-items: center; gap: 4px; margin-top: 3px; }
       |.$answering { display: flex; align-items: center; gap: 4px; }
       |.$answering .end { font-size: 11px; color: rgba(255, 255, 255, 0.75); }
       |button { all: initial; min-width: 10px; text-align: center; padding: 1px 6px; border-radius: 4px; background: rgba(255, 255, 255, 0.18); color: #fff; font: 500 11px/1.4 system-ui, sans-serif; cursor: pointer; }
       |button:hover { background: rgba(255, 255, 255, 0.32); }
       |button.$chosen { background: #fff; color: #0f766e; }
       |button:focus-visible { outline: 2px solid #fff; outline-offset: 1px; }
       |.more { margin-left: auto; }
       |""".stripMargin

  /**
    * The parts of the card of `headword`, as it shows at once: its dictionary
    * form and part of speech, its tip, but where that is the word, and its
    * buttons, which give `rate` each answer, with more shown if `detailed`.
    */
  def slots
    (
      headword: Headword,
      rate: Rating => Unit,
      detailed: Boolean,
    )
    : Slots =
    val slots = Slots(
      element("div", "card"),
      element("div", "head"),
      element("div", "entry"),
      element("div", "foot"),
    )
    slots.card.classList.toggle("detailed", detailed)
    heading(
      slots.head,
      headword.word,
      headword.pos,
      Nil,
      None,
    )
    slots.foot.appendChild(answers(headword, rate))
    List(
      Some(slots.head),
      Option.when(headword.tip.trim != headword.word.trim)(
        text("div", "tip", headword.tip),
      ),
      Some(slots.entry),
      Some(slots.foot),
    ).flatten.foreach(slots.card.appendChild)
    slots

  /**
    * Heads a card in `head` with `word`, its part of speech, `pos`, and its
    * `genders`, if known, and how it is said, `ipa`, if known.
    */
  def heading
    (
      head: Element,
      word: String,
      pos: String,
      genders: List[String],
      ipa: Option[String],
    )
    : Unit =
    clear(head)
    List(
      Some("word" -> word),
      Some(grammar(pos, genders)).filter(_.nonEmpty).map("pos" -> _),
      ipa.map("ipa" -> _),
    ).flatten
      .foreach((kind, content) => head.appendChild(text("span", kind, content)))

  /**
    * What the card of `headword` is called for a reader of the screen: its
    * dictionary form, and its part of speech, if known.
    */
  def label(headword: Headword): String =
    if headword.pos.isEmpty then headword.word
    else s"${ headword.word }, ${ part(headword.pos) }"

  /**
    * What to look `headword` up as: its dictionary form and part of speech, in
    * the sense of its meaning, cut to [[Lookup.longest]], as it is only weighed
    * against the dictionary's senses.
    */
  def lookup(headword: Headword): Lookup = Lookup(
    headword.word,
    headword.pos,
    headword.meaning.take(Lookup.longest),
    headword.language,
  )

  /** A part of speech, `pos`, as Kaikki names it, in full. */
  def part(pos: String): String = pos match
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
    * A word's part of speech, `pos`, in full, and its `genders`, as a card
    * heads it, as `noun, feminine`, or `noun, masculine or feminine` for a word
    * that may take either.
    */
  def grammar(pos: String, genders: List[String]): String =
    List(part(pos), genders.mkString(" or ")).filter(_.nonEmpty).mkString(", ")

  /**
    * What a card shows of `card` before its buttons, in order: the word's
    * principal forms, its senses, and what is shown only while the reader asks
    * for more.
    */
  def contents(card: Card): List[Element] =
    val more = details(card)
    List(
      Option.when(card.principal.nonEmpty)(forms(card.principal)),
      Option.when(card.entry.senses.nonEmpty)(senses(card.entry.senses)),
      Option.when(more.nonEmpty)(holding("div", "details", more)),
    ).flatten

  /** Whether there is more of `card` to show than a card shows at first. */
  def more(card: Card): Boolean = card.entry.senses.size > first ||
    card.entry.senses.exists(_.examples.nonEmpty) || card.tables.nonEmpty ||
    card.entry.etymology.nonEmpty

  /** The principal forms of a word, each with what it is. */
  private def forms(principal: List[(String, String)]): Element = holding(
    "div",
    "forms",
    principal.map((name, form) =>
      holding(
        "span",
        "form",
        List(
          text("span", "label", s"$name "),
          text("b", "", form),
        ),
      ),
    ),
  )

  /**
    * The senses of a word, each with its examples, those after the [[first]]
    * shown, and the examples, only while the reader asks for more.
    */
  private def senses(senses: List[Sense]): Element = holding(
    "ol",
    "senses",
    senses
      .zipWithIndex
      .map((sense, index) =>
        holding(
          "li",
          if index < first then "" else "extra",
          text(
            "span",
            "gloss",
            sense.glosses.mkString("; "),
          ) :: sense.examples.take(examples).map(example),
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

  /** What a card shows of `card` only while the reader asks for more. */
  private def details(card: Card): List[Element] = card.tables.map(table) ++
    card.entry.etymology.map(text("div", "etymology", _))

  /**
    * A table of a word's forms, which scrolls within the card if it is wider,
    * as a verb's of many tenses is.
    */
  private def table(table: Table): Element = holding(
    "div",
    "scroll",
    List(holding(
      "table",
      "",
      List(
        text("caption", "", table.title),
        holding(
          "thead",
          "",
          List(columns(table.columns)),
        ),
        holding("tbody", "", table.rows.map(row)),
      ),
    )),
  )

  /** The row of a table that names its `columns`, each heading its own. */
  private def columns(columns: List[String]): Element = holding(
    "tr",
    "",
    element("td", "") ::
      columns.map(column => scoped(text("th", "", column), "col")),
  )

  /** A row of a table, its label heading its cells. */
  private def row(row: Row): Element = holding(
    "tr",
    "",
    scoped(text("th", "", row.label), "row") :: row.cells.map(text("td", "", _)),
  )

  /** `cell`, a header of a table, of the `scope` it heads. */
  private def scoped(cell: Element, scope: String): Element =
    cell.setAttribute("scope", scope)
    cell

  /**
    * Where the entry of `card` is from: Wiktionary, under its licence, put into
    * the reader's language by their model if it was; or else their model.
    */
  def source(card: Card): Element = (card.entry.source, card.page) match
    case (Source.Wiktionary(_, translated), Some(page)) =>
      val link = text("a", "", "Wiktionary")
      link.setAttribute("href", page)
      link.setAttribute("target", "_blank")
      link.setAttribute("rel", "noopener noreferrer")
      holding(
        "div",
        "source",
        List(
          text("span", "", "From "),
          link,
          text(
            "span",
            "",
            if translated then ", CC BY-SA, translated by your model"
            else ", CC BY-SA",
          ),
        ),
      )
    case _ => text(
        "div",
        "source",
        "Written by your model, so it may have mistakes.",
      )

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
