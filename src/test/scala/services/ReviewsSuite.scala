package com.alecdorrington.codeswitch.services

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.alecdorrington.codeswitch.lexicon.{
  Derivation, Dictionary, Editing, Example, Explanation, Lexeme, LexemeId, Page,
  Piece, Reason, Revising, Revision, Sense, SenseId, Source, Spelling, Store,
  Stores, Timestamp,
}
import com.alecdorrington.codeswitch.models.{Kaikki, Lookup, Wiktionary}
import com.alecdorrington.codeswitch.services.Translator.{
  Fitted, Merged, Reviewed,
}
import com.alecdorrington.iris.{Model, ModelClient}
import com.alecdorrington.megalingual.Language
import io.circe.Json
import munit.CatsEffectSuite
import scala.concurrent.duration.DurationInt
import sttp.client4.impl.cats.CatsMonadAsyncError
import sttp.client4.testing.{BackendStub, ResponseStub}

class ReviewsSuite extends CatsEffectSuite:

  private val german  = Language.German
  private val english = Language.English
  private val model   = Model.ClaudeHaiku4_5

  /** As Wiktionary's record of `Bank` gave it, at `at`. */
  private def sourced(at: Long): Source.Sourced = Source.Sourced(
    Dictionary.Wiktionary,
    Wiktionary.url(german, "Bank"),
    Timestamp(at),
  )

  /** `Bank`, the bench, as Wiktionary gave it, with four senses. */
  private val bench = Derivation
    .fromKaikki(
      german,
      Kaikki.lines("Bank"),
      sourced(0),
    )
    .head

  /** What a model generated on a page, met as `bench`. */
  private val generated = Source.Generated(
    model,
    Nil,
    Some(Page("https://example.com/", "A park")),
    Timestamp(1),
  )

  /**
    * `bench`, with a sense that a model generated from a page, which repeats
    * its first, and a second definition of its first sense that repeats it.
    */
  private val repeating = bench.copy(senses =
    bench
      .senses
      .head
      .copy(definitions =
        bench.senses.head.definitions :+ Explanation.of(
          english,
          "a bench to sit on",
          sourced(0),
        ),
      ) ::
      (
        bench.senses.tail :+ Sense(
          SenseId(bench.id, "page-en-1", 4),
          Set(german),
          Set.empty,
          List(Explanation.of(
            english,
            "a seat for several people",
            generated,
          )),
          List(Example(
            "Wir sitzen auf der Bank.",
            Explanation.of(
              english,
              "We sit on the bench.",
              generated,
            ),
            None,
            generated,
          )),
          Nil,
          generated,
        )
      ),
  )

  /** `repeating`'s parts, by their ids. */
  private val named = Reviews.parts(repeating)

  test(
    "A review merges each group of parts that repeat each other into one in the place of the first, credited to the records of what it merged, takes out what is no longer needed, all as one change, and undoing it gives the lexeme back.",
  ):
    val reviewed = Reviewed(
      List(Merged(
        List("s1", "s5"),
        List("bench; pew; a seat for several people"),
        None,
        None,
      )),
      List("s3"),
    )
    val after = Reviews
      .applied(
        repeating,
        named,
        reviewed,
        english,
        model,
        Timestamp(9),
      )
      .get
    val merged = after.senses.head
    assertEquals(
      after.senses.map(_.definitions.head.versions.head.text),
      List(
        "bench; pew; a seat for several people",
        repeating.senses(1).definitions.head.versions.head.text,
        repeating.senses(3).definitions.head.versions.head.text,
      ),
    )
    assertEquals(
      merged.source,
      Source.Generated(
        model,
        List(sourced(0)),
        None,
        Timestamp(9),
      ),
    )
    assertEquals(
      merged.examples,
      repeating.senses(4).examples,
    )
    assertEquals(
      after.rejected.map(rejected => (rejected.piece.place, rejected.reason)),
      List(
        0 -> Reason.Reviewed,
        4 -> Reason.Reviewed,
        2 -> Reason.Reviewed,
      ),
    )
    assertEquals(
      Editing.undone(after, Timestamp(9)),
      repeating,
    )

  test(
    "A review merges definitions, or examples, of one sense, but nothing of a group of parts of different kinds or senses, of fewer than two, or of parts not there, and never takes every sense out.",
  ):
    val definitions = Reviews
      .applied(
        repeating,
        named,
        Reviewed(
          List(Merged(
            List("s1d1", "s1d2"),
            Nil,
            Some("a bench"),
            None,
          )),
          Nil,
        ),
        english,
        model,
        Timestamp(9),
      )
      .get
    assertEquals(
      definitions.senses.head.definitions.map(_.versions.head.text),
      List("a bench"),
    )
    assertEquals(
      definitions.senses.head.definitions.head.versions.head.source,
      Source.Generated(
        model,
        List(sourced(0)),
        None,
        Timestamp(9),
      ),
    )
    assertEquals(
      Reviews.applied(
        repeating,
        named,
        Reviewed(
          List(
            Merged(
              List("s1", "s1d1"),
              List("a"),
              Some("a"),
              None,
            ),
            Merged(
              List("s1d1", "s2d1"),
              Nil,
              Some("a"),
              None,
            ),
            Merged(List("s1"), List("a"), None, None),
            Merged(List("s1", "s9"), List("a"), None, None),
            Merged(List("s1", "s2"), Nil, None, None),
          ),
          Nil,
        ),
        english,
        model,
        Timestamp(9),
      ),
      None,
    )
    val spared = Reviews
      .applied(
        repeating,
        named,
        Reviewed(
          Nil,
          List("s1", "s2", "s3", "s4", "s5", "s5e1"),
        ),
        english,
        model,
        Timestamp(9),
      )
      .get
    assertEquals(spared.senses.size, 5)
    assertEquals(spared.senses(4).examples, Nil)

  test(
    "Asked of a meaning met on a page, a model's saying that a sense kept has it adds nothing, and a sense it writes otherwise goes last, from the page.",
  ):
    val page = Some(Page("https://example.com/", "A park"))
    assertEquals(
      Reviews.sensed(
        bench,
        Fitted(Some("s2"), None),
        english,
        page,
        model,
        Timestamp(3),
      ),
      Left(1),
    )
    val written = Reviews.sensed(
      bench,
      Fitted(
        None,
        Some(
          com
            .alecdorrington
            .codeswitch
            .models
            .Sense(List(" a park bench ", " "), Nil),
        ),
      ),
      english,
      page,
      model,
      Timestamp(3),
    )
    assertEquals(
      written.map(_.map(piece =>
        piece match
          case Piece.Meaning(sense, place) =>
            (place, sense.definitions.map(_.versions.head.text), sense.source)
          case _ => (0, Nil, Source.Edited(Timestamp(0))),
      )),
      Right(Some((
        bench.senses.size,
        List("a park bench"),
        Source.Generated(model, Nil, page, Timestamp(3)),
      ))),
    )

  /** A model that answers every request with `text`, counting requests. */
  private def answering(text: String, asked: Ref[IO, Int]): Lexicon.Ask = [A] =>
    (use: (Model, ModelClient[IO]) => IO[A]) =>
      asked.update(_ + 1) *> use(
        model,
        ModelClient[IO](
          Translator.config(model, "key"),
          BackendStub(CatsMonadAsyncError[IO]())
            .whenAnyRequest
            .thenRespond(ResponseStub.adjust(
              Json
                .obj(
                  "content" -> Json.arr(Json.obj(
                    "type" -> Json.fromString("text"),
                    "text" -> Json.fromString(text),
                  )),
                  "stop_reason" -> Json.fromString("end_turn"),
                  "usage"       -> Json.obj(
                    "input_tokens"  -> Json.fromInt(10),
                    "output_tokens" -> Json.fromInt(5),
                  ),
                )
                .noSpaces,
            )),
        ),
      )

  /** `lexeme`, kept in a new store, as its spelling's only one. */
  private def keeping(lexeme: Lexeme): IO[Store] = Stores
    .fresh
    .flatTap(_.keep(List(lexeme), Nil, Nil))

  test(
    "A model reviews a lexeme kept, and what it finds is kept as one change; a sense added from a page is first checked against those kept, then the lexeme is reviewed.",
  ):
    for
      store    <- keeping(repeating)
      asked    <- Ref[IO].of(0)
      reviewed <- Reviews.review(
        repeating.id,
        english,
        store,
        answering(
          """{"merges": [{"parts": ["s1", "s5"], "definitions": ["a bench"], "text": null, "translation": null}], "unneeded": []}""",
          asked,
        ),
      )
      kept    <- store.lexemes(List(repeating.id))
      reviews <- Ref[IO].of(List.empty[String])
      fits    <- Revisions
        .revised(
          Revising(bench.id, Revision.Add("seat", None)),
          english,
          store,
          answering(
            """{"fits": "s1", "sense": null}""",
            asked,
          ),
          id => reviews.update(_ :+ id.text),
        )
        .attempt
      added <- Revisions.revised(
        Revising(
          bench.id,
          Revision.Add(
            "a bench in a park",
            Some(Page("https://example.com/", "A park")),
          ),
        ),
        english,
        store,
        answering(
          """{"fits": null, "sense": {"glosses": ["a park bench"], "examples": []}}""",
          asked,
        ),
        id => reviews.update(_ :+ id.text),
      )
      reviewedIds <- reviews.get
    yield
      assertEquals(reviewed, kept.headOption)
      assertEquals(
        kept.map(_.senses.head.definitions.head.versions.head.text),
        List("a bench"),
      )
      assertEquals(
        fits.left.map(_.getMessage),
        Left("Sense 1 already has that meaning."),
      )
      assertEquals(
        added.senses.last.definitions.head.versions.head.text,
        "a park bench",
      )
      assertEquals(reviewedIds, List(bench.id.text))

  test(
    "A lexeme whose record is fetched again is reviewed if it held what the reader or a model put in it, but not if it held only what the record gave.",
  ):
    val lookup = Lookup("Bank", "noun", "bench", german)
    val fetch  = (_: String) => IO.pure(Some(Kaikki.lines("Bank")))
    val again  = (store: Store) =>
      store
        .homographs(List(Spelling(german, "Bank")))
        .flatMap(listed =>
          store.keep(
            Nil,
            listed.map(_.copy(checked = Map.empty)),
            Nil,
          ),
        )
    val reworded = (lexeme: Lexeme) =>
      val piece = Piece.Definition(
        lexeme.senses.head.id,
        lexeme.senses.head.definitions.head,
        0,
      )
      Editing
        .reworded(
          piece,
          "a seat",
          None,
          english,
          Timestamp(5),
        )
        .flatMap(made =>
          Editing.replaced(
            lexeme,
            List(piece),
            List(made),
            Timestamp(5),
            Reason.Reworded,
          ),
        )
        .toList
    for
      storage <- Memory(_ => false, 0.millis)
      store   <- Stores.fresh
      reviews <- Ref[IO].of(List.empty[String])
      look = (_: Unit) =>
        Lexicon.entry(
          english,
          lookup,
          store,
          storage,
          fetch,
          answering("", Ref.unsafe(0)),
          id => reviews.update(_ :+ id.text),
        )
      _         <- look(())
      _         <- again(store)
      _         <- look(())
      untouched <- reviews.get
      kept      <- store.lexemes(List(bench.id))
      _         <- store.keep(kept.flatMap(reworded), Nil, Nil)
      _         <- again(store)
      _         <- look(())
      edited    <- reviews.get
    yield
      assertEquals(untouched, Nil)
      assertEquals(edited, List(bench.id.text))

  /**
    * `bench`, its first sense with four definitions, each of Wiktionary's
    * record.
    */
  private val defined = bench.copy(senses =
    bench
      .senses
      .head
      .copy(definitions =
        bench.senses.head.definitions ++ List("a pew", "a seat", "a form").map(
          Explanation.of(english, _, sourced(0)),
        ),
      ) :: bench.senses.tail,
  )

  /** The texts of each sense of `lexeme`, as first given. */
  private def texts(lexeme: Lexeme): List[List[String]] = lexeme
    .senses
    .map(_.definitions.flatMap(_.versions.headOption).map(_.text))

  test(
    "Two merges of one kind keep their order, each merged part where the first of its group was, however the model orders them, and undoing them gives the lexeme back.",
  ):
    val senses = Reviews
      .applied(
        repeating,
        named,
        Reviewed(
          List(
            Merged(List("s3", "s4"), List("two"), None, None),
            Merged(List("s1", "s2"), List("one"), None, None),
          ),
          Nil,
        ),
        english,
        model,
        Timestamp(9),
      )
      .get
    assertEquals(
      texts(senses),
      List(
        List("one"),
        List("two"),
        texts(repeating)(4),
      ),
    )
    assertEquals(
      Editing.undone(senses, Timestamp(9)),
      repeating,
    )
    val definitions = Reviews
      .applied(
        defined,
        Reviews.parts(defined),
        Reviewed(
          List(
            Merged(
              List("s1d4", "s1d3"),
              Nil,
              Some("b"),
              None,
            ),
            Merged(
              List("s1d2", "s1d1"),
              Nil,
              Some("a"),
              None,
            ),
          ),
          Nil,
        ),
        english,
        model,
        Timestamp(9),
      )
      .get
    assertEquals(texts(definitions).head, List("a", "b"))
    assertEquals(
      Editing.undone(definitions, Timestamp(9)),
      defined,
    )

  test(
    "A review never merges or takes out what the reader wrote, nor a sense that holds it, which its model is shown with no id.",
  ):
    val first = Piece.Definition(
      repeating.senses.head.id,
      repeating.senses.head.definitions.head,
      0,
    )
    val theirs = Editing
      .reworded(
        first,
        "a long seat",
        None,
        english,
        Timestamp(2),
      )
      .flatMap(made =>
        Editing.replaced(
          repeating,
          List(first),
          List(made),
          Timestamp(2),
          Reason.Reworded,
        ),
      )
      .get
    val parts = Reviews.parts(theirs).map(_._1)
    assert(
      !parts.contains("s1") && !parts.contains("s1d1"),
      parts,
    )
    assert(
      parts.contains("s1d2") && parts.contains("s5"),
      parts,
    )
    val shown = Reviews.context(theirs, english).hcursor.downField("senses")
    assertEquals(
      shown.downN(0).downField("id").focus,
      None,
    )
    assertEquals(
      shown.downN(0).downField("definitions").downN(0).focus,
      Some(Json.obj("text" -> Json.fromString("a long seat"))),
    )
    assertEquals(
      shown.downN(4).get[String]("id"),
      Right("s5"),
    )
    assertEquals(
      Reviews.applied(
        theirs,
        Reviews.parts(theirs),
        Reviewed(
          List(Merged(
            List("s1", "s5"),
            List("a bench"),
            None,
            None,
          )),
          List("s1d1"),
        ),
        english,
        model,
        Timestamp(9),
      ),
      None,
    )

  test(
    "A sense added at the reader's asking is a change of its own, which can be undone, but the making of a model's lexeme is none.",
  ):
    for
      store <- keeping(bench)
      added <- Revisions.revised(
        Revising(
          bench.id,
          Revision.Add("a seat in a park", None),
        ),
        english,
        store,
        answering(
          """{"fits": null, "sense": {"glosses": ["a park bench"], "examples": []}}""",
          Ref.unsafe(0),
        ),
        _ => IO.unit,
      )
      change = Editing.changes(added)
      undone <- Revisions.revised(
        Revising(
          bench.id,
          Revision.Undo(change.head.at),
        ),
        english,
        store,
        answering("", Ref.unsafe(0)),
        _ => IO.unit,
      )
    yield
      assertEquals(
        change.map(change =>
          (change.reason, change.taken, change.made.map(_.place)),
        ),
        List((Reason.Added, Nil, List(bench.senses.size))),
      )
      assertEquals(undone, bench)
      assertEquals(Editing.changes(undone), Nil)
      assertEquals(
        Editing.changes(Derivation.generated(
          com
            .alecdorrington
            .codeswitch
            .models
            .Entry(
              "Bank",
              "noun",
              Nil,
              List(
                com.alecdorrington.codeswitch.models.Sense(List("a bench"), Nil),
              ),
              Nil,
              None,
              Nil,
              None,
              com.alecdorrington.codeswitch.models.Source.Model,
            ),
          german,
          english,
          model,
          Timestamp(4),
        )),
        Nil,
      )
