package com.alecdorrington.codeswitch.views

import cats.effect.IO
import com.alecdorrington.codeswitch.Codeswitch
import com.alecdorrington.codeswitch.lexicon.{
  Attributed, Explanation, Form, Lexeme, LexemeId, Piece, Rejected, Revising,
  Revision, Sense, Source, Timestamp,
}
import com.alecdorrington.codeswitch.models.{learnable, Learner}
import com.alecdorrington.codeswitch.services.Revisions
import com.alecdorrington.megalingual.Language
import com.alecdorrington.megalingual.Language.English
import com.alecdorrington.omnibrowser.{Browser, Granted, Page}
import com.alecdorrington.omnibrowser.Permission.Storage
import com.alecdorrington.omnibrowser.laminar.*
import com.raquo.laminar.api.L.*
import org.scalajs.dom
import scala.concurrent.duration.DurationInt
import scala.scalajs.js
import scala.scalajs.js.URIUtils.{decodeURIComponent, encodeURIComponent}
import scala.scalajs.js.timers.setTimeout

object Editor:

  /**
    * The editor, the extension's options page, which Laminar renders, styled by
    * `editor.css`.
    */
  def page(using Granted[Storage]): Page = LaminarPage(Editor(_).view)
    .withStylesheets("editor.css")

  /** The longest own of a lexeme that the editor is opened at. */
  val longest = 300

  /** The address of the editor of the lexeme whose own is `id`. */
  def address(id: String): String = s"options.html#${ encodeURIComponent(id) }"

  /**
    * Who gave a claim from `source`, as the editor says: a dictionary, a model
    * translating or writing for the reader, or the reader.
    */
  def credit(source: Source): String = source match
    case Source.Sourced(dictionary, _, _) => dictionary.title
    case Source.Translated(from, _, _, _) =>
      s"${ from.dictionary.title }, translated by your model"
    case Source.Generated(_, _, _, Nil)  => "Your model"
    case Source.Generated(_, _, _, from) => s"Your model, from ${ from
          .map(_.dictionary.title)
          .distinct
          .mkString(" and ") }"
    case Source.Edited(_) => "You"

  /**
    * The title of a table of forms laid out in `template`, as a source names it
    * (`declension`), or a model's (`model`).
    */
  def titleOf(template: String): String =
    if template == "model" then "Forms by your model" else template.capitalize

  /**
    * What the change made at `at` did, as `made`, every claim of a lexeme and
    * of what was taken out of it, shows: wrote something of the reader's, had a
    * model write something, or only took something out.
    */
  def change(at: Timestamp, made: List[Attributed]): String = made
    .map(_.source)
    .filter(_.change.contains(at))
    .collectFirst {
      case Source.Edited(_)             => "Reworded"
      case Source.Generated(_, _, _, _) => "Written again by your model"
    }
    .getOrElse("Deleted")

/**
  * The editor of a lexeme of the reader's lexicon, which the Edit button of a
  * word's card opens in a tab of its own, naming the lexeme in its address:
  * every piece of it, each with where it came from, and buttons that reword it
  * in place, take it out, or have the reader's model write it again, told what
  * is wrong with it, if they say; and each change made, which may be undone,
  * putting back what it took out. The background makes each change, as it keeps
  * the lexicon, and the editor shows the lexeme as it then is.
  */
private[codeswitch] final class Editor
  (browser: Browser)
  (using Granted[Storage]):

  /** The own of the lexeme to edit, from the address, if it names one. */
  private val id: Option[LexemeId] = Some(dom.window.location.hash.drop(1))
    .filter(_.nonEmpty)
    .map(text => LexemeId.read(decodeURIComponent(text)))

  /** The reader's own language, as chosen, or else as it is taken to be. */
  private val native = browser
    .signal(Codeswitch.native)
    .combineWith(browser.signal(Codeswitch.learning))
    .map((native, learning) =>
      native.fold(Learner.own(learning.map(_.language)))(learnable.resolve),
    )

  /** The lexeme, once read, or none if the lexicon keeps none of its own. */
  private val lexeme = Var(Option.empty[Option[Lexeme]])

  /** The piece being reworded or written again, if any. */
  private val open = Var(Option.empty[Opened])

  /** Whether a change is being made. */
  private val busy = Var(false)

  /** What the editor last said of a change. */
  private val status = Var("")

  def view: HtmlElement = div(
    cls := "editor",
    onMountCallback(_ => load()),
    child <--
      lexeme
        .signal
        .combineWith(native, open.signal, busy.signal)
        .map((found, native, opened, busy) =>
          found.fold(p(cls := "note", "Looking it up…"))(_.fold(missing)(
            lexeme => entry(Shown(lexeme, native, opened, busy)),
          )),
        ),
    p(
      cls  := "status",
      role := "status",
      child.text <-- status.signal,
    ),
  )

  /** Reads the lexeme, from the background. */
  private def load(): Unit = browser.run(
    id.fold(IO(lexeme.set(Some(None))))(id =>
      browser
        .runtime
        .call(Revisions.lexeme)(id)
        .attempt
        .flatMap(outcome => IO(outcome.fold(failed, found => show(found)))),
    ),
  )

  /** Shows `found`, as the lexeme now is, naming the page for its word. */
  private def show(found: Option[Lexeme]): Unit =
    lexeme.set(Some(found))
    dom.document.title = found.fold("Your lexicon")(lexeme =>
      s"${ lexeme.lemma.spelling.text } · Your lexicon",
    )

  /** What the editor shows of no lexeme. */
  private def missing: HtmlElement = div(
    h1("Your lexicon"),
    p(
      cls := "note",
      "This word is not in your lexicon. Open a word's entry from its card, with Edit, to change it here.",
    ),
  )

  /** The lexeme of `shown`, all of it, and the changes made to it. */
  private def entry(shown: Shown): HtmlElement =
    val lexeme = shown.lexeme
    val learnt = Revisions.languageOf(lexeme, shown.native)
    div(
      headerTag(
        h1(
          lang := learnt.code,
          lexeme.lemma.spelling.text,
        ),
        p(
          cls := "about",
          s"${ learnt.title(English) } ${ Cards.inFull(lexeme.wordClass.name) }",
        ),
      ),
      p(
        cls := "note",
        "Reword, delete or regenerate any part of this entry. Your model writes a part again from the rest of the entry, and from what you tell it is wrong. Each change can be undone, under Changes.",
      ),
      part("senses", "Senses")(
        if lexeme.senses.isEmpty then p(cls := "note", "None.")
        else
          ol(
            cls := "senses",
            lexeme.senses.zipWithIndex.map(meaning(shown, _, _)),
          ),
      ),
      part("etymology", "Etymology")(pieces(
        lexeme
          .etymologies
          .zipWithIndex
          .map((etymology, place) =>
            worded(
              shown,
              s"o$place",
              s"etymology ${ place + 1 }",
              Piece.Origin(etymology, place),
              etymology,
            ),
          ),
      )),
      part("forms", "Forms")(
        if lexeme.forms.isEmpty then p(cls := "note", "None.")
        else
          div(tables(lexeme.forms).zipWithIndex.map(table(shown, learnt, _, _))),
      ),
      part("changes", "Changes")(changes(shown)),
    )

  /** A section of the entry named `name`, its heading of `key`. */
  private def part(key: String, name: String)(content: HtmlElement) =
    sectionTag(
      aria.labelledBy := s"$key-heading",
      h2(
        idAttr   := s"$key-heading",
        tabIndex := -1,
        name,
      ),
      content,
    )

  /** A list of `items`, or a note that there are none. */
  private def pieces(items: List[HtmlElement]): HtmlElement =
    if items.isEmpty then p(cls := "note", "None.")
    else ul(cls                 := "pieces", items)

  /** The sense at `place`, with its definitions and examples, and buttons. */
  private def meaning(shown: Shown, sense: Sense, place: Int): HtmlElement =
    val (key, name) = (s"s$place", s"sense ${ place + 1 }")
    val learnt      = Revisions.languageOf(shown.lexeme, shown.native)
    li(
      cls    := "sense",
      idAttr := key,
      div(
        cls := "line",
        span(cls := "label", s"Sense ${ place + 1 }"),
        credit(sense.source),
        actions(
          shown,
          key,
          name,
          List(Piece.Meaning(sense, place)),
          rewordable = false,
        ),
      ),
      regenerating(
        shown,
        key,
        name,
        List(Piece.Meaning(sense, place)),
      ),
      h3("Definitions"),
      pieces(
        sense
          .definitions
          .zipWithIndex
          .map((definition, at) =>
            worded(
              shown,
              s"$key-d$at",
              s"definition ${ at + 1 } of $name",
              Piece.Definition(sense.id, definition, at),
              definition,
            ),
          ),
      ),
      h3("Examples"),
      pieces(
        sense
          .examples
          .zipWithIndex
          .map((example, at) =>
            item(
              shown,
              s"$key-e$at",
              s"example ${ at + 1 } of $name",
              Piece.Usage(sense.id, example, at),
              Text(example.text, Some(learnt)),
              Some(
                Revisions.shown(example.translation, shown.native).getOrElse(""),
              ),
              example.source,
            ),
          ),
      ),
    )

  /** An explanation, `explanation`, as the reader reads it, a piece. */
  private def worded
    (
      shown: Shown,
      key: String,
      name: String,
      piece: Piece,
      explanation: Explanation,
    )
    : HtmlElement =
    val version = explanation
      .read(shown.native)
      .orElse(explanation.versions.headOption)
    item(
      shown,
      key,
      name,
      piece,
      Text(
        version.fold("")(_.text),
        version.map(_.language),
      ),
      None,
      version.fold(Source.Edited(Timestamp(0)))(_.source),
    )

  /**
    * The piece `piece`, keyed `key` and called `name`, showing `text`, and, of
    * an example, its translation, from `source`, with buttons, or the form that
    * rewords it, while it is open.
    */
  private def item
    (
      shown: Shown,
      key: String,
      name: String,
      piece: Piece,
      text: Text,
      translation: Option[String],
      source: Source,
    )
    : HtmlElement = li(
    cls    := "piece",
    idAttr := key,
    if shown.opened.contains(Opened(key, Mode.Reword)) then
      rewording(key, name, piece, text.text, translation)
    else
      div(
        div(
          cls := "line",
          span(
            cls := "text",
            text.language.map(lang := _.code),
            text.text,
          ),
          translation
            .filter(_.nonEmpty)
            .map(translation => span(cls := "translation", s"— $translation")),
          credit(source),
          actions(
            shown,
            key,
            name,
            List(piece),
            regenerable = !piece.isInstanceOf[Piece.Inflection],
          ),
        ),
        regenerating(shown, key, name, List(piece)),
      ),
  )

  /** Who gave a claim from `source`. */
  private def credit(source: Source) =
    span(cls := "source", Editor.credit(source))

  /**
    * The buttons of the piece or pieces `pieces`, keyed `key` and called
    * `name`, that reword it, if `rewordable`, have the model write it again, if
    * `regenerable`, as a sense, a text or a table, but not a form alone, and
    * take it out.
    */
  private def actions
    (
      shown: Shown,
      key: String,
      name: String,
      pieces: List[Piece],
      rewordable: Boolean = true,
      regenerable: Boolean = true,
    )
    : HtmlElement = div(
    cls := "actions",
    Option.when(rewordable)(
      action(
        shown,
        s"$key-edit",
        "Edit",
        s"Edit $name",
      )(() => opening(Opened(key, Mode.Reword), s"$key-text")),
    ),
    Option.when(regenerable)(
      action(
        shown,
        s"$key-regenerate",
        "Regenerate",
        s"Regenerate $name",
      )(() =>
        opening(
          Opened(key, Mode.Regenerate),
          s"$key-note",
        ),
      ),
    ),
    action(
      shown,
      s"$key-delete",
      "Delete",
      s"Delete $name",
    )(() =>
      revise(
        Revision.Delete(pieces),
        "Deleted. It can be undone, under Changes.",
        List(
          s"$key-edit",
          s"$key-regenerate",
          s"$key-delete",
          "changes-heading",
        ),
      ),
    ),
  )

  /** A button, `text`, named `label`, that does `act`, but not while busy. */
  private def action
    (
      shown: Shown,
      id: String,
      text: String,
      label: String,
    )
    (act: () => Unit)
    : HtmlElement = button(
    tpe        := "button",
    idAttr     := id,
    aria.label := label,
    disabled   := shown.busy,
    text,
    onClick --> (_ => act()),
  )

  /**
    * The form that rewords the piece keyed `key`, called `name`, of `text`,
    * and, of an example, its `translation`.
    */
  private def rewording
    (
      key: String,
      name: String,
      piece: Piece,
      text: String,
      translation: Option[String],
    )
    : HtmlElement = form(
    cls := "rewording",
    onSubmit.preventDefault -->
      (_ =>
        revise(
          Revision.Reword(
            piece,
            valueOf(s"$key-text"),
            translation.map(_ => valueOf(s"$key-translation")),
          ),
          "Saved.",
          List(s"$key-edit"),
        )
      ),
    onKeyDown.filter(_.key == "Escape") --> (_ => closing(s"$key-edit")),
    label(forId := s"$key-text", name.capitalize),
    piece match
      case Piece.Inflection(_, _) =>
        input(idAttr := s"$key-text", value := text)
      case _ => textArea(
          idAttr := s"$key-text",
          rows   := 2,
          value  := text,
        ),
    translation.map(translation =>
      List(
        label(
          forId := s"$key-translation",
          "Its translation",
        ),
        textArea(
          idAttr := s"$key-translation",
          rows   := 2,
          value  := translation,
        ),
      ),
    ),
    div(
      cls := "actions",
      button(tpe := "submit", "Save"),
      button(
        tpe := "button",
        "Cancel",
        onClick --> (_ => closing(s"$key-edit")),
      ),
    ),
  )

  /**
    * The form that has the model write `pieces`, keyed `key` and called `name`,
    * again, with what the reader says is wrong, while it is open.
    */
  private def regenerating
    (
      shown: Shown,
      key: String,
      name: String,
      pieces: List[Piece],
    )
    : Option[HtmlElement] = Option.when(
    shown.opened.contains(Opened(key, Mode.Regenerate)),
  )(form(
    cls := "regenerating",
    onSubmit.preventDefault -->
      (_ =>
        revise(
          Revision.Regenerate(pieces, valueOf(s"$key-note")),
          "Written again. It can be undone, under Changes.",
          List(s"$key-regenerate"),
        )
      ),
    onKeyDown.filter(_.key == "Escape") --> (_ => closing(s"$key-regenerate")),
    label(
      forId := s"$key-note",
      s"What is wrong with $name? Your model is told, if you say.",
    ),
    input(
      idAttr   := s"$key-note",
      tpe      := "text",
      disabled := shown.busy,
    ),
    div(
      cls := "actions",
      button(
        tpe      := "submit",
        disabled := shown.busy,
        "Regenerate",
      ),
      button(
        tpe      := "button",
        disabled := shown.busy,
        "Cancel",
        onClick --> (_ => closing(s"$key-regenerate")),
      ),
    ),
  ))

  /**
    * Each table of `forms`, of the table that it was laid out in, in the order
    * of the first of each, with each form's place among them.
    */
  private def tables
    (forms: List[Form])
    : List[(Option[String], List[(Form, Int)])] =
    val placed = forms.zipWithIndex
    placed
      .map(_._1.table)
      .distinct
      .map(table => table -> placed.filter(_._1.table == table))

  /**
    * The table at `index` of the forms of `laid`, those of the table that they
    * were laid out in, if any, as it names them, or else the principal forms.
    */
  private def table
    (
      shown: Shown,
      learnt: Language,
      laid: (Option[String], List[(Form, Int)]),
      index: Int,
    )
    : HtmlElement =
    val (template, forms) = laid
    val title             = template.fold("Principal forms")(Editor.titleOf)
    val (key, name)       = (s"t$index", title.toLowerCase)
    val all = forms.map((form, place) => Piece.Inflection(form, place))
    div(
      cls := "table",
      div(
        cls := "line",
        h3(title),
        actions(
          shown,
          key,
          s"the $name",
          all,
          rewordable = false,
        ),
      ),
      regenerating(shown, key, name, all),
      pieces(
        forms
          .zipWithIndex
          .map { case ((form, place), at) =>
            item(
              shown,
              s"$key-f$at",
              s"form ${ at + 1 } of the $name",
              Piece.Inflection(form, place),
              Text(form.spelling.text, Some(learnt)),
              None,
              form.source,
            ).amend(span(
              cls := "tags",
              form.tags.map(_.name).toList.sorted.mkString(" "),
            ))
          },
      ),
    )

  /**
    * The changes made to the lexeme of `shown`, the latest first, each with
    * what it took out, and a button that undoes it.
    */
  private def changes(shown: Shown): HtmlElement =
    val lexeme = shown.lexeme
    val made   = lexeme.claims ++ lexeme.rejected.flatMap(_.piece.claims)
    val each   = lexeme.rejected.groupBy(_.at).toList.sortBy(-_._1.millis)
    if each.isEmpty then
      p(
        cls := "note",
        "None yet. Each change you make here can be undone.",
      )
    else
      ol(
        cls := "changes",
        each.map((at, taken) =>
          val said = s"${ Editor.change(at, made) }, ${ when(at) }"
          li(
            div(
              cls := "line",
              span(cls := "label", said),
              button(
                tpe        := "button",
                idAttr     := s"c${ at.millis }-undo",
                aria.label := s"Undo: $said",
                disabled   := shown.busy,
                "Undo",
                onClick -->
                  (_ =>
                    revise(
                      Revision.Undo(at),
                      "Undone.",
                      List("changes-heading"),
                    )
                  ),
              ),
            ),
            ul(
              cls := "taken",
              taken.map(taken => li(described(taken, shown.native))),
            ),
          ),
        ),
      )

  /** What was taken out, as `rejected` keeps it. */
  private def described(rejected: Rejected, native: Language): String =
    val read = (explanation: Explanation) =>
      Revisions.shown(explanation, native).getOrElse("")
    rejected.piece match
      case Piece.Meaning(sense, _) =>
        s"Sense: ${ sense.definitions.map(read).mkString("; ") }"
      case Piece.Definition(_, definition, _) =>
        s"Definition: ${ read(definition) }"
      case Piece.Usage(_, example, _) => s"Example: ${ example.text }"
      case Piece.Origin(etymology, _) => s"Etymology: ${ read(etymology) }"
      case Piece.Inflection(form, _)  => s"Form: ${ form.spelling.text }"

  /** When `at` was, as the reader's browser says it, to the minute. */
  private def when(at: Timestamp): String = new js.Date(at.millis.toDouble)
    .asInstanceOf[js.Dynamic]
    .toLocaleString(
      js.undefined,
      js.Dynamic.literal(dateStyle = "medium", timeStyle = "short"),
    )
    .asInstanceOf[String]

  /** Opens `opened`, giving `field` the focus. */
  private def opening(opened: Opened, field: String): Unit =
    open.set(Some(opened))
    focusing(List(field))

  /** Closes what is open, giving the focus back to `back`. */
  private def closing(back: String): Unit =
    open.set(None)
    focusing(List(back))

  /**
    * Has the background make `revision`, then shows the lexeme as it is, says
    * `done`, and gives the first of `next` that is in the page the focus; or
    * says why it could not be made.
    */
  private def revise
    (
      revision: Revision,
      done: String,
      next: List[String],
    )
    : Unit = id.foreach(id =>
    busy.set(true)
    status.set(working(revision))
    browser.run(
      browser
        .runtime
        .call(Revisions.revise)(Revising(id, revision))
        .attempt
        .flatMap(outcome => IO(made(outcome, done, next))),
    ),
  )

  /**
    * Shows how a change went, by `outcome`: the lexeme as it is then, saying
    * `done`, the focus given to the first of `next` in the page; or why not.
    */
  private def made
    (
      outcome: Either[Throwable, Lexeme],
      done: String,
      next: List[String],
    )
    : Unit =
    busy.set(false)
    outcome.fold(
      failed,
      revised =>
        open.set(None)
        show(Some(revised))
        status.set(done)
        focusing(next),
    )

  /** What the editor says while `revision` is being made. */
  private def working(revision: Revision): String = revision match
    case Revision.Regenerate(_, _) => "Your model is writing it again…"
    case _                         => "Saving…"

  /**
    * Says why a change could not be made, reading the lexeme again if it
    * changed meanwhile.
    */
  private def failed(error: Throwable): Unit =
    val reason = Codeswitch.reason(error)
    status.set(reason)
    if reason == Revisions.changed.message then
      open.set(None)
      load()

  /** Gives the first of `ids` that is in the page the focus, once shown. */
  private def focusing(ids: List[String]): Unit = setTimeout(0.millis)(
    ids
      .map(dom.document.getElementById)
      .find(_ != null)
      .foreach(_.asInstanceOf[dom.HTMLElement].focus()),
  )

  /** The value of the field whose own is `id`. */
  private def valueOf(id: String): String =
    dom.document.getElementById(id) match
      case field: dom.HTMLInputElement    => field.value
      case field: dom.HTMLTextAreaElement => field.value
      case _                              => ""

/** What the editor shows: `lexeme`, for a reader of `native`, as it is. */
private final case class Shown
  (
    lexeme: Lexeme,
    native: Language,
    opened: Option[Opened],
    busy: Boolean,
  )

/** The piece of the editor keyed `key`, open to be changed as `mode` says. */
private final case class Opened(key: String, mode: Mode)

/** How a piece is changed: by the reader's rewording, or by a model. */
private enum Mode:
  case Reword, Regenerate

/** A text of a lexeme, `text`, in `language`, if known. */
private final case class Text(text: String, language: Option[Language])
