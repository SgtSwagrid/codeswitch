package com.alecdorrington.codeswitch.services

import cats.effect.IO
import cats.syntax.all.*
import com.alecdorrington.codeswitch.lexicon.{
  Editing, Example, Explanation, Form, Lexeme, LexemeId, Page, Piece, Reason,
  Revising, Revision, Sense, SenseId, Source, Spelling, Store, Tag, Timestamp,
}
import com.alecdorrington.codeswitch.models
import com.alecdorrington.iris.{Model, ModelClient}
import com.alecdorrington.megalingual.Language
import com.alecdorrington.omnibrowser.Message
import io.circe.Json
import io.circe.syntax.*

/**
  * Changes the reader's lexicon as they edit it, in its editor: taking pieces
  * out of a lexeme, rewording them, having a model write them again, and
  * undoing each change (see [[Editing]]). Only the background uses it, through
  * the editor's messages, which keeps the lexicon out of the pages.
  */
object Revisions:

  /**
    * The lexeme whose own is given, as the reader's lexicon keeps it, if it
    * does.
    */
  val lexeme = Message[LexemeId, Option[Lexeme]]("lexeme")

  /**
    * Revises a lexeme of the reader's lexicon as they ask, giving it as it is
    * then, or fails with why it could not be.
    */
  val revise = Message[Revising, Lexeme]("revise")

  /** The most characters that the reader may write in place of a piece. */
  val longest = 1000

  /**
    * The lexeme that `revising` names, as kept in `store`, revised as it asks
    * for a reader of `native`, by a model that `ask` gives, if it asks for one,
    * then kept, and given to `review` if it gained a sense that a model
    * generated; or a failure, if it is not kept, holds no longer what is to be
    * revised, as when it changed meanwhile, or the model fails.
    */
  def revised
    (
      revising: Revising,
      native: Language,
      store: Store,
      ask: Lexicon.Ask,
      review: LexemeId => IO[Unit],
    )
    : IO[Lexeme] = revising.revision match
    case Revision.Regenerate(pieces, note) => regenerated(
        revising.lexeme,
        pieces,
        note,
        native,
        store,
        ask,
      ).flatTap(_ =>
        review(revising.lexeme).whenA(pieces.exists {
          case Piece.Meaning(_, _) => true
          case _                   => false
        }),
      )
    case Revision.Add(meaning, page) => IO.raiseWhen(
        blank(meaning) || meaning.length > longest,
      )(Problem("There is no meaning to add.")) *> added(
        revising.lexeme,
        meaning,
        page,
        native,
        store,
        ask,
      ).flatTap(_ => review(revising.lexeme))
    case Revision.Reword(_, text, translation) if blank(text) =>
      IO.raiseError(Problem("Write something in its place."))
    case Revision.Reword(_, text, translation)
      if (text :: translation.toList).exists(_.length > longest) =>
      IO.raiseError(Problem("That is too long."))
    case revision => Timestamp
        .now
        .flatMap(at =>
          changing(revising.lexeme, store)(applied(_, revision, native, at)),
        )

  /**
    * `lexeme` revised by `revision`, made at `at`, for a reader of `native`, or
    * none if it no longer holds what is to be revised.
    */
  private def applied
    (
      lexeme: Lexeme,
      revision: Revision,
      native: Language,
      at: Timestamp,
    )
    : Option[Lexeme] = revision match
    case Revision.Delete(pieces) => Editing.deleted(lexeme, pieces, at)
    case Revision.Reword(piece, text, translation) => Editing
        .located(lexeme, piece)
        .flatMap(found =>
          Editing.reworded(
            found,
            text.trim,
            translation.map(_.trim).filter(_.nonEmpty),
            wordedIn(found, native),
            at,
          ),
        )
        .flatMap(made =>
          Editing.replaced(
            lexeme,
            List(piece),
            List(made),
            at,
            Reason.Reworded,
          ),
        )
    case Revision.Undo(change)     => Some(Editing.undone(lexeme, change))
    case Revision.Regenerate(_, _) => None
    case Revision.Add(_, _)        => None

  /**
    * The language that the reader read `piece` in, as a reader of `native`, in
    * which they reword it: theirs, or else English, if it was shown in that.
    */
  private def wordedIn(piece: Piece, native: Language): Language = (piece match
    case Piece.Definition(_, definition, _) => definition.read(native)
    case Piece.Origin(etymology, _)         => etymology.read(native)
    case Piece.Usage(_, example, _)         => example.translation.read(native)
    case _                                  => None
  ).fold(native)(_.language)

  /** Whether `text` is blank. */
  private def blank(text: String): Boolean = text.trim.isEmpty

  /**
    * The lexeme whose own is `id`, as kept in `store`, with `pieces` in it
    * written again by a model that `ask` gives, for a reader of `native`, told
    * `note`, then kept: a piece, or the forms of one table.
    */
  private def regenerated
    (
      id: LexemeId,
      pieces: List[Piece],
      note: String,
      native: Language,
      store: Store,
      ask: Lexicon.Ask,
    )
    : IO[Lexeme] =
    for
      _ <- IO.raiseWhen(note.length > longest)(Problem("That is too long."))
      lexeme <- kept(id, store)
      found  <- IO.fromOption(
        pieces.traverse(Editing.located(lexeme, _)).filter(regenerable),
      )(changed)
      at   <- Timestamp.now
      made <- ask((model, client) =>
        rewritten(
          client,
          lexeme,
          found,
          note,
          native,
          model,
          at,
        ),
      )
      revised <- changing(id, store)(
        Editing.replaced(_, found, made, at, Reason.Regenerated),
      )
    yield revised

  /**
    * The lexeme whose own is `id`, as kept in `store`, with a sense of
    * `meaning`, met on `page`, that a model that `ask` gives writes, for a
    * reader of `native`, at its end, then kept; but first the model is asked
    * whether a sense kept already has that meaning, and if one does, the lexeme
    * is left as it is, and the failure says which.
    */
  private def added
    (
      id: LexemeId,
      meaning: String,
      page: Option[Page],
      native: Language,
      store: Store,
      ask: Lexicon.Ask,
    )
    : IO[Lexeme] =
    for
      lexeme <- kept(id, store)
      at     <- Timestamp.now
      sensed <- ask((model, client) =>
        Translator
          .fitted(
            client,
            languageOf(lexeme, native),
            native,
            Reviews.meaningOf(lexeme, meaning, native),
          )
          .map(Reviews.sensed(lexeme, _, native, page, model, at)),
      )
      made <- sensed.fold(
        place =>
          IO.raiseError(Problem(s"Sense ${ place +
              1 } already has that meaning.")),
        IO.fromOption(_)(Problem.unwritten),
      )
      revised <- changing(id, store)(lexeme =>
        Some(Editing.added(
          lexeme,
          List(made.at(lexeme.senses.size)),
        )),
      )
    yield revised

  /** Whether a model may write `pieces` again: one piece, or forms of a table. */
  private def regenerable(pieces: List[Piece]): Boolean = pieces match
    case List(_)                                   => true
    case forms @ (Piece.Inflection(first, _) :: _) => forms.forall:
        case Piece.Inflection(form, _) => form.table == first.table
        case _                         => false
    case _ => false

  /**
    * The pieces that `client`, of `model`, writes in place of `pieces` of
    * `lexeme` at `at`, for a reader of `native`, told `note`, each of the
    * source that credits what `pieces` drew on.
    */
  private def rewritten
    (
      client: ModelClient[IO],
      lexeme: Lexeme,
      pieces: List[Piece],
      note: String,
      native: Language,
      model: Model,
      at: Timestamp,
    )
    : IO[List[Piece]] =
    val learnt  = languageOf(lexeme, native)
    val context = Revisions.context(lexeme, pieces, note, native)
    val source  = Source.Generated(
      model,
      None,
      at,
      pieces.flatMap(_.claims).flatMap(_.source.credits).distinct,
    )
    val explained = (text: String) => Explanation.of(native, text, source)
    pieces match
      case List(Piece.Definition(sense, _, place)) => Translator
          .rewrittenDefinition(client, learnt, native, context)
          .map(text => List(Piece.Definition(sense, explained(text), place)))
      case List(Piece.Origin(_, place)) => Translator
          .rewrittenEtymology(client, learnt, native, context)
          .map(text => List(Piece.Origin(explained(text), place)))
      case List(Piece.Usage(sense, _, place)) => Translator
          .rewrittenExample(client, learnt, native, context)
          .map(example =>
            List(Piece.Usage(
              sense,
              exampleOf(example, explained, source),
              place,
            )),
          )
      case List(Piece.Meaning(old, place)) => Translator
          .rewrittenSense(client, learnt, native, context)
          .map(sense =>
            List(Piece.Meaning(
              Sense(
                SenseId(
                  lexeme.id,
                  s"model-${ native.code }-${ at.millis }",
                  place,
                ),
                old.languages,
                Set.empty,
                sense.glosses.map(explained),
                sense.examples.map(exampleOf(_, explained, source)),
                Nil,
                source,
              ),
              place,
            )),
          )
      case forms => Translator
          .rewrittenForms(client, learnt, native, context)
          .map(
            _.zipWithIndex
              .map((form, index) =>
                Piece.Inflection(
                  Form(
                    Spelling(learnt, form.form),
                    Set(learnt),
                    form.tags.map(Tag(_)).toSet,
                    forms
                      .collectFirst { case Piece.Inflection(old, _) =>
                        old.table
                      }
                      .flatten,
                    Nil,
                    source,
                  ),
                  forms.head.place + index,
                ),
              ),
          )

  /** The example of `example`, its translation explained by `explained`. */
  private def exampleOf
    (
      example: models.Example,
      explained: String => Explanation,
      source: Source,
    )
    : Example = Example(
    example.text,
    example.translation.fold(Explanation.none)(explained),
    None,
    source,
  )

  /**
    * What a model is told of `lexeme` to write `pieces` of it again, for a
    * reader of `native`, with their `note`: the lexeme as it stands, each text
    * as they read it, and which part to write again, by its place, counted from
    * `1`, with its text.
    */
  def context
    (
      lexeme: Lexeme,
      pieces: List[Piece],
      note: String,
      native: Language,
    )
    : Json =
    val read = (explanation: Explanation) => shown(explanation, native)
    Json
      .obj(
        "word"      -> lexeme.lemma.spelling.text.asJson,
        "wordClass" -> lexeme.wordClass.name.asJson,
        "senses"    ->
          lexeme
            .senses
            .map(sense =>
              Json.obj(
                "definitions" -> sense.definitions.flatMap(read).asJson,
                "examples"    -> sense.examples.map(example(_, native)).asJson,
              ),
            )
            .asJson,
        "etymologies" -> lexeme.etymologies.flatMap(read).asJson,
        "forms"       -> lexeme.forms.map(form).asJson,
        "rewrite"     -> part(lexeme, pieces, native),
        "note"        -> Some(note.trim).filter(_.nonEmpty).asJson,
      )
      .dropNullValues

  /**
    * Which part of `lexeme` `pieces` are, by place, counted from `1`, with what
    * they hold.
    */
  private def part
    (
      lexeme: Lexeme,
      pieces: List[Piece],
      native: Language,
    )
    : Json =
    val read    = (explanation: Explanation) => shown(explanation, native)
    val senseOf = (id: SenseId) => lexeme.senses.indexWhere(_.id == id) + 1
    pieces match
      case List(Piece.Meaning(_, place)) => Json.obj(
          "part"  -> "sense".asJson,
          "sense" -> (place + 1).asJson,
        )
      case List(Piece.Definition(sense, definition, place)) => Json.obj(
          "part"       -> "definition".asJson,
          "sense"      -> senseOf(sense).asJson,
          "definition" -> (place + 1).asJson,
          "text"       -> read(definition).asJson,
        )
      case List(Piece.Usage(sense, example, place)) => Json.obj(
          "part"    -> "example".asJson,
          "sense"   -> senseOf(sense).asJson,
          "example" -> (place + 1).asJson,
          "text"    -> this.example(example, native),
        )
      case List(Piece.Origin(etymology, place)) => Json.obj(
          "part"      -> "etymology".asJson,
          "etymology" -> (place + 1).asJson,
          "text"      -> read(etymology).asJson,
        )
      case forms => Json.obj(
          "part"  -> "forms".asJson,
          "forms" ->
            forms.collect { case Piece.Inflection(old, _) => form(old) }.asJson,
        )

  /** An example as the reader reads it, with its translation, if any. */
  private def example(example: Example, native: Language): Json = Json
    .obj(
      "text"        -> example.text.asJson,
      "translation" -> shown(example.translation, native).asJson,
    )
    .dropNullValues

  /** A form, with what it is. */
  private def form(form: Form): Json = Json.obj(
    "form" -> form.spelling.text.asJson,
    "tags" -> form.tags.map(_.name).toList.sorted.asJson,
  )

  /**
    * The text of `explanation` that a reader of `native` sees in the editor: in
    * their language, or else English, or else as it was first given.
    */
  def shown
    (
      explanation: Explanation,
      native: Language,
    )
    : Option[String] = explanation
    .read(native)
    .orElse(explanation.versions.headOption)
    .map(_.text)

  /** The language of `lexeme`, or else `native`, if it names none. */
  def languageOf(lexeme: Lexeme, native: Language): Language = lexeme
    .lemma
    .languages
    .headOption
    .getOrElse(native)

  /**
    * The lexeme whose own is `id`, as kept in `store`, changed by `change` and
    * kept, its spellings listed, one change to the lexicon at a time, or a
    * failure if it is not kept, or `change` gives none, as when it no longer
    * holds what was to be changed.
    */
  private[services] def changing
    (id: LexemeId, store: Store)
    (change: Lexeme => Option[Lexeme])
    : IO[Lexeme] = Lexicon
    .lock
    .permit
    .surround(
      kept(id, store)
        .flatMap(lexeme => IO.fromOption(change(lexeme))(changed))
        .flatTap(lexeme =>
          Lexicon
            .listing(store, List(lexeme), Nil, identity)
            .flatMap(store.keep(List(lexeme), _, Nil)),
        ),
    )

  /** The lexeme whose own is `id`, as kept in `store`, or a failure if none is. */
  private[services] def kept(id: LexemeId, store: Store): IO[Lexeme] = store
    .lexemes(List(id))
    .map(_.headOption)
    .flatMap(IO.fromOption(_)(Problem("This word is not in your lexicon.")))

  /** That the lexeme changed meanwhile, as in another tab. */
  val changed =
    Problem("This entry changed meanwhile. It is shown as it is now.")
