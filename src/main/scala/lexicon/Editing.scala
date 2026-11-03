package com.alecdorrington.codeswitch.lexicon

import cats.syntax.all.*
import com.alecdorrington.megalingual.Language
import io.circe.Codec

/**
  * How the reader, or a model in their place, changes a lexeme: by taking
  * pieces out of it, by putting others in their place, and by undoing a change.
  * Each piece taken out is kept as [[Rejected]], so that nothing a source gave
  * is lost, and a source that gives it again does not bring it back.
  */
object Editing:

  /**
    * `lexeme` with `pieces` taken out by the change made at `at`, or none if
    * one of them is not in it, as when the lexeme changed meanwhile.
    */
  def deleted
    (
      lexeme: Lexeme,
      pieces: List[Piece],
      at: Timestamp,
    )
    : Option[Lexeme] = replaced(lexeme, pieces, Nil, at, Reason.Deleted)

  /**
    * `lexeme` with `made` put in place of `pieces` by the change made at `at`,
    * for `reason`, or none if one of them is not in it: each of `pieces` taken
    * out, and kept as rejected at its place, and each of `made` put at its own
    * in what is left.
    */
  def replaced
    (
      lexeme: Lexeme,
      pieces: List[Piece],
      made: List[Piece],
      at: Timestamp,
      reason: Reason,
    )
    : Option[Lexeme] = Option
    .when(pieces.nonEmpty)(pieces)
    .flatMap(_.traverse(located(lexeme, _)))
    .map(found =>
      added(found.foldLeft(lexeme)(removed), made).copy(rejected =
        lexeme.rejected ++ found.map(Rejected(_, at, reason)),
      ),
    )

  /** `lexeme` with `made` put in it, each at its place. */
  def added(lexeme: Lexeme, made: List[Piece]): Lexeme =
    made.foldLeft(lexeme)(inserted)

  /**
    * `lexeme` with the change made at `at` undone: each later change to what it
    * made undone first, the latest first, then what it made taken out, and what
    * it took out put back in its place.
    */
  def undone(lexeme: Lexeme, at: Timestamp): Lexeme =
    val restored     = later(lexeme, at).foldLeft(lexeme)(undone)
    val (back, kept) = restored.rejected.partition(_.at == at)
    back
      .map(_.piece)
      .sortBy(_.place)
      .foldLeft(unmade(restored, at))(inserted)
      .copy(rejected = kept)

  /**
    * The moments of the changes after the one made at `at` that took out what
    * it made, the latest first.
    */
  private def later(lexeme: Lexeme, at: Timestamp): List[Timestamp] = lexeme
    .rejected
    .filter(rejected =>
      rejected.at.millis > at.millis &&
      rejected.piece.claims.exists(_.source.change.contains(at)),
    )
    .map(_.at)
    .distinct
    .sortBy(-_.millis)

  /** `lexeme` without what the change made at `at` put in it. */
  private def unmade(lexeme: Lexeme, at: Timestamp): Lexeme =
    val made      = (claim: Attributed) => claim.source.change.contains(at)
    val explained =
      (explanation: Explanation) => explanation.versions.exists(made)
    lexeme.copy(
      senses = lexeme
        .senses
        .filterNot(made)
        .map(sense =>
          sense.copy(
            definitions = sense.definitions.filterNot(explained),
            examples = sense.examples.filterNot(made),
          ),
        ),
      etymologies = lexeme.etymologies.filterNot(explained),
      forms = lexeme.forms.filterNot(made),
    )

  /**
    * `piece` as the reader reworded it at `at`, in `language`, to say `text`,
    * and, of an example, its translation, `translation`, if any; or none for a
    * sense, whose parts are reworded instead.
    */
  def reworded
    (
      piece: Piece,
      text: String,
      translation: Option[String],
      language: Language,
      at: Timestamp,
    )
    : Option[Piece] =
    val source    = Source.Edited(at)
    val explained = (text: String) => Explanation.of(language, text, source)
    piece match
      case Piece.Meaning(_, _)            => None
      case Piece.Definition(id, _, place) =>
        Some(Piece.Definition(id, explained(text), place))
      case Piece.Usage(id, _, place) => Some(Piece.Usage(
          id,
          Example(
            text,
            translation.fold(Explanation.none)(explained),
            None,
            source,
          ),
          place,
        ))
      case Piece.Origin(_, place) => Some(Piece.Origin(explained(text), place))
      case Piece.Inflection(form, place) => Some(Piece.Inflection(
          form.copy(
            spelling = Spelling(
              form.languages.headOption.getOrElse(language),
              text,
            ),
            pronunciations = Nil,
            source = source,
          ),
          place,
        ))

  /**
    * The changes made to `lexeme` that may be undone, the latest first: each
    * that took something out, and each that only put something in, an addition,
    * but not the making of the lexeme itself, as a model's lexeme is all of
    * claims of its own moment.
    */
  def changes(lexeme: Lexeme): List[Change] =
    val own = lexeme.lemma.source.change
    (lexeme.rejected.map(_.at) ++ lexeme.claims.flatMap(_.source.change))
      .distinct
      .filterNot(own.contains)
      .sortBy(-_.millis)
      .map(at =>
        val taken = lexeme.rejected.filter(_.at == at)
        Change(
          at,
          taken.headOption.fold(Reason.Added)(_.reason),
          taken.map(_.piece),
          madeAt(lexeme, at),
        ),
      )

  /**
    * The pieces of `lexeme` that the change made at `at` put in it, a sense
    * whole, with what it holds, else each definition, example, etymology and
    * form.
    */
  def madeAt(lexeme: Lexeme, at: Timestamp): List[Piece] =
    val made      = (claim: Attributed) => claim.source.change.contains(at)
    val explained =
      (explanation: Explanation) => explanation.versions.exists(made)
    lexeme
      .senses
      .zipWithIndex
      .flatMap((sense, place) =>
        if made(sense) then List(Piece.Meaning(sense, place))
        else
          sense
            .definitions
            .zipWithIndex
            .collect {
              case (definition, at) if explained(definition) =>
                Piece.Definition(sense.id, definition, at)
            } ++
            sense
              .examples
              .zipWithIndex
              .collect {
                case (example, at) if made(example) =>
                  Piece.Usage(sense.id, example, at)
              },
      ) ++
      lexeme
        .etymologies
        .zipWithIndex
        .collect {
          case (etymology, place) if explained(etymology) =>
            Piece.Origin(etymology, place)
        } ++
      lexeme
        .forms
        .zipWithIndex
        .collect {
          case (form, place) if made(form) => Piece.Inflection(form, place)
        }

  /** `piece` at its place in `lexeme` now, if it is in it. */
  def located(lexeme: Lexeme, piece: Piece): Option[Piece] = Some(
    piece match
      case Piece.Meaning(sense, _)             => lexeme.senses.indexOf(sense)
      case Piece.Definition(id, definition, _) =>
        senseOf(lexeme, id).fold(-1)(_.definitions.indexOf(definition))
      case Piece.Usage(id, example, _) =>
        senseOf(lexeme, id).fold(-1)(_.examples.indexOf(example))
      case Piece.Origin(etymology, _) => lexeme.etymologies.indexOf(etymology)
      case Piece.Inflection(form, _)  => lexeme.forms.indexOf(form),
  ).filter(_ >= 0).map(piece.at)

  /** The sense of `lexeme` whose own is `id`, if it has one. */
  def senseOf(lexeme: Lexeme, id: SenseId): Option[Sense] = lexeme
    .senses
    .find(_.id == id)

  /** `lexeme` without `piece`. */
  private def removed(lexeme: Lexeme, piece: Piece): Lexeme =
    changed(lexeme, piece)([A] =>
      (list: List[A], item: A, _: Int) => list.diff(List(item)),
    )

  /** `lexeme` with `piece` at its place, or last if there are fewer. */
  private def inserted(lexeme: Lexeme, piece: Piece): Lexeme =
    changed(lexeme, piece)([A] =>
      (list: List[A], item: A, place: Int) =>
        list.patch(
          place.max(0).min(list.size),
          List(item),
          0,
        ),
    )

  /**
    * `lexeme` with the list that holds pieces of the kind of `piece` changed by
    * `change`, given what `piece` holds and its place.
    */
  private def changed
    (lexeme: Lexeme, piece: Piece)
    (change: [A] => (List[A], A, Int) => List[A])
    : Lexeme = piece match
    case Piece.Meaning(sense, place) =>
      lexeme.copy(senses = change(lexeme.senses, sense, place))
    case Piece.Definition(id, definition, place) => within(lexeme, id)(sense =>
        sense.copy(definitions = change(sense.definitions, definition, place)),
      )
    case Piece.Usage(id, example, place) => within(lexeme, id)(sense =>
        sense.copy(examples = change(sense.examples, example, place)),
      )
    case Piece.Origin(etymology, place) =>
      lexeme.copy(etymologies = change(lexeme.etymologies, etymology, place))
    case Piece.Inflection(form, place) =>
      lexeme.copy(forms = change(lexeme.forms, form, place))

  /** `lexeme` with its sense whose own is `id` changed by `change`. */
  private def within
    (lexeme: Lexeme, id: SenseId)
    (change: Sense => Sense)
    : Lexeme = lexeme.copy(senses =
    lexeme.senses.map(sense => if sense.id == id then change(sense) else sense),
  )

/** What the reader changes in a lexeme, from its editor. */
enum Revision derives Codec:

  /** Takes `pieces` out. */
  case Delete(pieces: List[Piece])

  /**
    * Puts the reader's `text` in place of `piece`, and, of an example, its
    * `translation`, if any.
    */
  case Reword
    (
      piece: Piece,
      text: String,
      translation: Option[String],
    )

  /**
    * Has a model write `pieces` again, one piece, or the forms of one table,
    * told the reader's `note` on what is wrong with them, if it is not blank.
    */
  case Regenerate(pieces: List[Piece], note: String)

  /** Undoes the change made at `at`, and every later change to what it made. */
  case Undo(at: Timestamp)

  /**
    * Adds a sense of `meaning`, in which the reader met the word on `page`, if
    * no sense kept has it already, written by a model.
    */
  case Add(meaning: String, page: Option[Page])

/**
  * A change made to a lexeme at `at`, for `reason`: what it took out, `taken`,
  * and what it put in that is in the lexeme still, `made`.
  */
final case class Change
  (
    at: Timestamp,
    reason: Reason,
    taken: List[Piece],
    made: List[Piece],
  )

/** A revision of the lexeme whose own is `lexeme`. */
final case class Revising(lexeme: LexemeId, revision: Revision) derives Codec
