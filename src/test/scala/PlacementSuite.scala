package com.alecdorrington.codeswitch

import com.alecdorrington.codeswitch.Place.*
import munit.FunSuite

class PlacementSuite extends FunSuite:

  /**
    * The text that `pieces` of the translation of `word` make of `text`, all of
    * which is one text node, or nothing if they are shown nowhere.
    */
  private def placed
    (text: String, word: String, pieces: Piece*)
    : Option[String] =
    lay(text, Span(0, text.length), word, pieces*).map(render(text, _))

  private def lay
    (
      text: String,
      node: Span,
      word: String,
      pieces: Piece*,
    )
    : Option[Layout] =
    val start = text.indexOf(word)
    Placement.lay(
      text,
      node,
      Span(start, start + word.length),
      Translation(pieces.toList),
    )

  /** `text`, with what `layout` shows in it. */
  private def render(text: String, layout: Layout): String =
    ((layout.home, layout.shown) ::
      layout.space.toList.map(_ -> "") ++
      layout
        .inserted
        .map(insert =>
          Span(insert.at, insert.at) ->
            (if insert.after then s" ${ insert.piece }"
             else s"${ insert.piece } "),
        ))
      .sortBy((span, _) => -span.start)
      .foldLeft(text):
        case (text, (span, shown)) => text.patch(span.start, shown, span.length)

  test("A piece is shown in place of its word."):
    assertEquals(
      placed(
        "The cat sat.",
        "cat",
        Piece("chat", Home),
      ),
      Some("The chat sat."),
    )

  test("A piece may replace a short run of words around its word."):
    assertEquals(
      placed(
        "The apple is red.",
        "apple",
        Piece("der Apfel", Replacing("The apple")),
      ),
      Some("Der Apfel is red."),
    )

  test("A run of words must include the word, and be short."):
    assertEquals(
      placed(
        "The apple is red.",
        "apple",
        Piece("rot", Replacing("is red")),
      ),
      None,
    )
    assertEquals(
      placed(
        "The apple is red.",
        "apple",
        Piece("x", Replacing("The apple is red")),
      ),
      None,
    )

  test("A run of words must be within the word's own text."):
    assertEquals(
      lay(
        "The apple is red.",
        Span(4, 9),
        "apple",
        Piece("der Apfel", Replacing("The apple")),
      ),
      None,
    )

  test("A piece moved after a word leaves no extra space in its place."):
    assertEquals(
      placed(
        "I have eaten the pie.",
        "eaten",
        Piece("gegessen", After("pie")),
      ),
      Some("I have the pie gegessen."),
    )
    assertEquals(
      placed(
        "Nobody has eaten.",
        "eaten",
        Piece("gegessen", After("Nobody")),
      ),
      Some("Nobody gegessen has."),
    )

  test("A word alone in its text, as in a link, leaves no extra space either."):
    val text = "She had never really understood."
    assertEquals(
      lay(
        text,
        Span(14, 20),
        "really",
        Piece("wirklich", After("understood")),
      ).map(render(text, _)),
      Some("She had never understood wirklich."),
    )

  test("A piece may be moved before a word."):
    assertEquals(
      placed(
        "I often eat fish.",
        "eat",
        Piece("esse", Before("often")),
      ),
      Some("I esse often fish."),
    )

  test("A word may be split into pieces in two places."):
    assertEquals(
      placed(
        "I get up early.",
        "get",
        Piece("stehe", Replacing("get up")),
        Piece("auf", After("early")),
      ),
      Some("I stehe early auf."),
    )

  test("Of the words a piece might follow, the nearest is meant."):
    assertEquals(
      placed(
        "The cat saw the dog and the bird.",
        "saw",
        Piece("sah", After("the")),
      ),
      Some("The cat the sah dog and the bird."),
    )

  test("A translation is shown nowhere if a piece's place is not found."):
    assertEquals(
      placed(
        "I ate. The pie was good.",
        "ate",
        Piece("aß", After("pie")),
      ),
      None,
    )
    assertEquals(
      placed(
        "I get up early.",
        "get",
        Piece("stehe", Home),
        Piece("auf", After("late")),
      ),
      None,
    )

  test("A translation is shown nowhere if its pieces disagree on a run."):
    assertEquals(
      placed(
        "I get up early.",
        "get",
        Piece("stehe", Replacing("get up")),
        Piece("ich", Replacing("I get")),
      ),
      None,
    )

  test("A declined translation is shown nowhere."):
    assertEquals(placed("Ask Alice.", "Alice"), None)
