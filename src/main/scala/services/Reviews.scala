package com.alecdorrington.codeswitch.services

import cats.effect.IO
import cats.syntax.all.*
import com.alecdorrington.codeswitch.lexicon.{
  Editing, Example, Explanation, Lexeme, LexemeId, Page, Piece, Reason,
  Revising, Sense, SenseId, Source, Store, Timestamp,
}
import com.alecdorrington.codeswitch.services.Translator.{
  Fitted, Merged, Reviewed,
}
import com.alecdorrington.iris.Model
import com.alecdorrington.megalingual.Language
import io.circe.Json
import io.circe.syntax.*

/**
  * Keeps the reader's lexicon free of repetition: a model reviews a lexeme when
  * it gains something new, a sense generated or a record fetched again, finding
  * what repeats, what could be merged, and what is no longer needed, which is
  * applied as one change, through the same mechanism as the reader's (see
  * [[Editing]]), each merged part credited to the records of what it merges;
  * and before a sense is generated from a page, a model is asked whether a
  * sense kept already has its meaning.
  */
object Reviews:

  /**
    * The parts of `lexeme` that a review may merge or take out, each by the id
    * it is shown under: each sense, `s1`, its definitions, `s1d1`, and its
    * examples, `s1e1`, and each etymology, `o1`.
    */
  def parts(lexeme: Lexeme): List[(String, Piece)] = lexeme
    .senses
    .zipWithIndex
    .flatMap((sense, place) =>
      val key = s"s${ place + 1 }"
      (key -> Piece.Meaning(sense, place)) ::
        sense
          .definitions
          .zipWithIndex
          .map((definition, at) =>
            s"${ key }d${ at + 1 }" ->
              Piece.Definition(sense.id, definition, at),
          ) ++
        sense
          .examples
          .zipWithIndex
          .map((example, at) =>
            s"${ key }e${ at + 1 }" -> Piece.Usage(sense.id, example, at),
          ),
    ) ++
    lexeme
      .etymologies
      .zipWithIndex
      .map((etymology, place) =>
        s"o${ place + 1 }" -> Piece.Origin(etymology, place),
      )

  /**
    * What a model is told of `lexeme`, for a reader of `native`, to review it:
    * each of its [[parts]] as they read it, by its id.
    */
  def context(lexeme: Lexeme, native: Language): Json =
    val read = (explanation: Explanation) =>
      Revisions.shown(explanation, native).getOrElse("")
    Json.obj(
      "word"      -> lexeme.lemma.spelling.text.asJson,
      "wordClass" -> lexeme.wordClass.name.asJson,
      "senses"    ->
        lexeme
          .senses
          .zipWithIndex
          .map((sense, place) =>
            val key = s"s${ place + 1 }"
            Json.obj(
              "id"          -> key.asJson,
              "definitions" ->
                sense
                  .definitions
                  .zipWithIndex
                  .map((definition, at) =>
                    Json.obj(
                      "id"   -> s"${ key }d${ at + 1 }".asJson,
                      "text" -> read(definition).asJson,
                    ),
                  )
                  .asJson,
              "examples" ->
                sense
                  .examples
                  .zipWithIndex
                  .map((example, at) =>
                    Json.obj(
                      "id"          -> s"${ key }e${ at + 1 }".asJson,
                      "text"        -> example.text.asJson,
                      "translation" -> read(example.translation).asJson,
                    ),
                  )
                  .asJson,
            ),
          )
          .asJson,
      "etymologies" ->
        lexeme
          .etymologies
          .zipWithIndex
          .map((etymology, place) =>
            Json.obj(
              "id"   -> s"o${ place + 1 }".asJson,
              "text" -> read(etymology).asJson,
            ),
          )
          .asJson,
    )

  /**
    * `lexeme` with what `reviewed` found in it, as its [[parts]] were, `named`,
    * applied as one change, made by `model` at `at`, for a reader of `native`:
    * each group of parts of one kind merged into the one part that takes their
    * place, credited to the records of what it merges, if it gives what that
    * part needs, and each part no longer needed taken out, but never every
    * sense, nor a part twice; or none if nothing it found can be applied, as of
    * parts no longer in it.
    */
  def applied
    (
      lexeme: Lexeme,
      named: List[(String, Piece)],
      reviewed: Reviewed,
      native: Language,
      model: Model,
      at: Timestamp,
    )
    : Option[Lexeme] =
    val byId   = named.toMap
    val groups = reviewed
      .merges
      .zipWithIndex
      .flatMap((merge, index) =>
        grouped(byId, merge).flatMap(pieces =>
          merged(
            lexeme,
            pieces,
            merge,
            native,
            Source.Generated(
              model,
              None,
              at,
              pieces.flatMap(_.claims).flatMap(_.source.credits).distinct,
            ),
            at,
            index,
          ).map(pieces -> _),
        ),
      )
      .foldLeft(List.empty[(List[Piece], Piece)])((kept, group) =>
        if group._1.exists(piece => kept.exists(_._1.contains(piece))) then kept
        else kept :+ group,
      )
    val taken = groups.flatMap(_._1)
    val gone  = spared(
      lexeme,
      reviewed.unneeded.distinct.flatMap(byId.get).filterNot(taken.contains),
      groups.map(_._1),
    )
    val removed = within(taken ++ gone)
    val made    = groups.collect {
      case (pieces, made) if pieces.forall(removed.contains) =>
        made.at(placeAfter(pieces, removed))
    }
    Editing.replaced(
      lexeme,
      removed,
      made,
      at,
      Reason.Reviewed,
    )

  /**
    * The pieces that `merge` names in `byId`, if it names at least two, all
    * there, and all of one kind, and of one sense if within one.
    */
  private def grouped
    (byId: Map[String, Piece], merge: Merged)
    : Option[List[Piece]] =
    val ids = merge.parts.distinct
    Some(ids.flatMap(byId.get)).filter(pieces =>
      pieces.size >= 2 && pieces.size == ids.size &&
      pieces.map(kindOf).distinct.size == 1,
    )

  /**
    * `unneeded`, but not every sense of `lexeme` that `groups` leave it with
    * none, as a review may never take every sense out.
    */
  private def spared
    (
      lexeme: Lexeme,
      unneeded: List[Piece],
      groups: List[List[Piece]],
    )
    : List[Piece] =
    val senses = unneeded.count(kindOf(_) == "sense")
    val merged = groups.count(_.headOption.exists(kindOf(_) == "sense"))
    if lexeme.senses.size - senses + merged > 0 then unneeded
    else unneeded.filterNot(kindOf(_) == "sense")

  /**
    * `pieces`, each once, but not a definition or an example of a sense among
    * them, which goes with its sense.
    */
  private def within(pieces: List[Piece]): List[Piece] =
    val senses = pieces.collect { case Piece.Meaning(sense, _) => sense.id }
    pieces
      .distinct
      .filterNot:
        case Piece.Definition(id, _, _) => senses.contains(id)
        case Piece.Usage(id, _, _)      => senses.contains(id)
        case _                          => false

  /**
    * Of what kind `piece` is, and so in which list it is: a sense, a definition
    * or example of its sense, an etymology or a form.
    */
  private def kindOf(piece: Piece): String = piece match
    case Piece.Meaning(_, _)        => "sense"
    case Piece.Definition(id, _, _) => s"definition ${ id.text }"
    case Piece.Usage(id, _, _)      => s"example ${ id.text }"
    case Piece.Origin(_, _)         => "etymology"
    case Piece.Inflection(_, _)     => "form"

  /**
    * Where the part that takes the place of `pieces` goes once `removed` are
    * taken out: where the first of them was, less those taken out before it.
    */
  private def placeAfter
    (
      pieces: List[Piece],
      removed: List[Piece],
    )
    : Int =
    val first = pieces.minBy(_.place)
    first.place - removed.count(piece =>
      kindOf(piece) == kindOf(first) && piece.place < first.place,
    )

  /**
    * The part that `merge` makes of `pieces`, of `lexeme`, from `source`, at
    * `at`, the `index`th of its review, in `native`, if it gives what one
    * needs.
    */
  private def merged
    (
      lexeme: Lexeme,
      pieces: List[Piece],
      merge: Merged,
      native: Language,
      source: Source,
      at: Timestamp,
      index: Int,
    )
    : Option[Piece] =
    val explained = (text: String) => Explanation.of(native, text.trim, source)
    val text      = merge.text.filter(_.trim.nonEmpty)
    pieces.head match
      case Piece.Meaning(_, _) =>
        val senses = pieces.collect { case Piece.Meaning(sense, _) => sense }
        Option
          .when(merge.definitions.exists(_.trim.nonEmpty))(Sense(
            SenseId(
              lexeme.id,
              s"review-${ at.millis }",
              index,
            ),
            senses.flatMap(_.languages).toSet,
            senses.flatMap(_.labels).toSet,
            merge.definitions.filter(_.trim.nonEmpty).map(explained),
            senses.flatMap(_.examples).distinctBy(_.text),
            senses.flatMap(_.relations).distinct,
            source,
          ))
          .map(Piece.Meaning(_, 0))
      case Piece.Definition(sense, _, _) =>
        text.map(text => Piece.Definition(sense, explained(text), 0))
      case Piece.Usage(sense, _, _) => text.map(text =>
          Piece.Usage(
            sense,
            Example(
              text.trim,
              merge
                .translation
                .filter(_.trim.nonEmpty)
                .fold(Explanation.none)(explained),
              None,
              source,
            ),
            0,
          ),
        )
      case Piece.Origin(_, _) =>
        text.map(text => Piece.Origin(explained(text), 0))
      case Piece.Inflection(_, _) => None

  /**
    * Has a model that `ask` gives review the lexeme whose own is `id`, as kept
    * in `store`, for a reader of `native`, and keeps what it finds, giving the
    * lexeme then, or none if it found nothing that could be applied; but if one
    * of it is already running, what that gives.
    */
  def review
    (
      id: LexemeId,
      native: Language,
      store: Store,
      ask: Lexicon.Ask,
    )
    : IO[Option[Lexeme]] = reviewing(id.text)(
    Revisions
      .kept(id, store)
      .flatMap(lexeme =>
        if parts(lexeme).size < 2 then IO.none
        else
          ask((model, client) =>
            Translator
              .reviewed(
                client,
                Revisions.languageOf(lexeme, native),
                native,
                context(lexeme, native),
              )
              .tupleLeft(model),
          ).flatMap((model, reviewed) =>
            Timestamp
              .now
              .flatMap(at =>
                Revisions
                  .changing(id, store)(applied(
                    _,
                    parts(lexeme),
                    reviewed,
                    native,
                    model,
                    at,
                  ))
                  .option,
              ),
          ),
      ),
  )

  /** The reviews running, by the lexeme each is of. */
  private val reviewing = Lexicon.Once[Option[Lexeme]]()

  /**
    * What a model is told of `lexeme`, for a reader of `native`, to say which
    * of its senses has `meaning`, the meaning in which they met the word: each
    * sense as they read it, by its id, and the meaning.
    */
  def meaningOf
    (
      lexeme: Lexeme,
      meaning: String,
      native: Language,
    )
    : Json = Json.obj(
    "word"      -> lexeme.lemma.spelling.text.asJson,
    "wordClass" -> lexeme.wordClass.name.asJson,
    "senses"    ->
      lexeme
        .senses
        .zipWithIndex
        .map((sense, place) =>
          Json.obj(
            "id"          -> s"s${ place + 1 }".asJson,
            "definitions" ->
              sense.definitions.flatMap(Revisions.shown(_, native)).asJson,
          ),
        )
        .asJson,
    "meaning" -> meaning.trim.asJson,
  )

  /**
    * The sense that `fitted`, by `model` at `at`, writes for `lexeme`, met on
    * `page`, in `native`, at its end, if no sense kept fits; or else the place
    * of the sense that does, which is kept.
    */
  def sensed
    (
      lexeme: Lexeme,
      fitted: Fitted,
      native: Language,
      page: Option[Page],
      model: Model,
      at: Timestamp,
    )
    : Either[Int, Option[Piece]] = fitted
    .fits
    .flatMap(id =>
      parts(lexeme).collectFirst { case (`id`, Piece.Meaning(_, place)) =>
        place
      },
    )
    .toLeft(
      fitted
        .sense
        .map(_.copy(glosses =
          fitted.sense.toList.flatMap(_.glosses).filter(_.trim.nonEmpty),
        ))
        .filter(_.glosses.nonEmpty)
        .map(sense =>
          val source = Source.Generated(model, page, at, Nil)
          val place  = lexeme.senses.size
          Piece.Meaning(
            Sense(
              SenseId(
                lexeme.id,
                s"page-${ native.code }-${ at.millis }",
                place,
              ),
              lexeme.lemma.languages,
              Set.empty,
              sense
                .glosses
                .map(gloss => Explanation.of(native, gloss.trim, source)),
              sense
                .examples
                .filter(_.text.trim.nonEmpty)
                .map(example =>
                  Example(
                    example.text.trim,
                    example
                      .translation
                      .filter(_.trim.nonEmpty)
                      .fold(Explanation.none)(
                        Explanation.of(native, _, source),
                      ),
                    None,
                    source,
                  ),
                ),
              Nil,
              source,
            ),
            place,
          ),
        ),
    )
