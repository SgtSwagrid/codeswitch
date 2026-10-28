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

  /**
    * `text`, with what `layout` shows in it, put in the order that
    * [[Highlighter]] puts it in.
    */
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
      .reverse
      .sortBy((span, _) => (-span.start, span.length == 0))
      .foldLeft(text):
        case (text, (span, shown)) => text.patch(span.start, shown, span.length)

  test(
    "A piece is shown in place of its word, or of a short run of words around it.",
  ):
    assertEquals(
      placed(
        "The cat sat.",
        "cat",
        Piece("chat", Home),
      ),
      Some("The chat sat."),
    )
    assertEquals(
      placed(
        "The apple is red.",
        "apple",
        Piece("der Apfel", Replacing("The apple")),
      ),
      Some("Der Apfel is red."),
    )

  test(
    "A run of words must include the word, be short, lie within the word's own text and hold no number, which the page keeps.",
  ):
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
    assertEquals(
      lay(
        "The apple is red.",
        Span(4, 9),
        "apple",
        Piece("der Apfel", Replacing("The apple")),
      ),
      None,
    )
    assertEquals(
      placed(
        "Total: 1,299.99 today",
        "Total",
        Piece("Summe", Replacing("Total: 1,299.99")),
      ),
      None,
    )

  test(
    "A piece may be moved after or before a word, and leaves no extra space in its place.",
  ):
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
    assertEquals(
      placed(
        "I often eat fish.",
        "eat",
        Piece("esse", Before("often")),
      ),
      Some("I esse often fish."),
    )

  test(
    "A word alone in its text, as in italics, or with no space beside it, leaves no extra space either, and a line break is never taken away.",
  ):
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
    assertEquals(
      placed(
        "They (ate) left.",
        "ate",
        Piece("aßen", After("left")),
      ),
      Some("They () left aßen."),
    )
    assertEquals(
      placed(
        "We eaten\npie.",
        "eaten",
        Piece("x", After("pie")),
      ),
      Some("We\npie x."),
    )

  test(
    "A word may be split into pieces in two places, one of which may be its own, and pieces put in one place are shown in their order.",
  ):
    assertEquals(
      placed(
        "I get up early.",
        "get",
        Piece("stehe", Replacing("get up")),
        Piece("auf", After("early")),
      ),
      Some("I stehe early auf."),
    )
    assertEquals(
      placed(
        "I get up early.",
        "get",
        Piece("stehe", Home),
        Piece("auf", After("early")),
      ),
      Some("I stehe up early auf."),
    )
    assertEquals(
      placed(
        "I ate.",
        "ate",
        Piece("a", After("I")),
        Piece("b", After("I")),
      ),
      Some("I a b."),
    )

  test(
    "Of the words a piece might follow, the nearest is meant, and of those as near as each other, the first.",
  ):
    assertEquals(
      placed(
        "The cat saw the dog and the bird.",
        "saw",
        Piece("sah", After("the")),
      ),
      Some("The cat the sah dog and the bird."),
    )
    assertEquals(
      placed(
        "One the sat the two.",
        "sat",
        Piece("X", After("the")),
      ),
      Some("One the X the two."),
    )

  test(
    "A translation is shown nowhere if it is declined, a piece's place is not found, or its pieces disagree on a run.",
  ):
    assertEquals(placed("Ask Alice.", "Alice"), None)
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
    assertEquals(
      placed(
        "I get up early.",
        "get",
        Piece("stehe", Replacing("get up")),
        Piece("ich", Replacing("I get")),
      ),
      None,
    )

  test(
    "A piece goes beside a whole word, which may hold a hyphen, and beside the word's place, but not within it.",
  ):
    assertEquals(
      placed(
        "It is a well-known fact.",
        "is",
        Piece("ist", After("well-known")),
      ),
      Some("It a well-known ist fact."),
    )
    assertEquals(
      placed(
        "It is a well-known fact.",
        "is",
        Piece("ist", After("well")),
      ),
      None,
    )
    assertEquals(
      placed(
        "I have eaten pie.",
        "eaten",
        Piece("gegessen", After("have")),
      ),
      Some("I have gegessen pie."),
    )
    assertEquals(
      placed(
        "I get up early.",
        "get",
        Piece("stehe", Replacing("get up")),
        Piece("auf", After("up")),
      ),
      None,
    )

  /**
    * What stands for the piece of a word moved earlier, as [[Highlighter]]
    * gives the text of a passage, in which the gaps that word left are nothing,
    * as they show nothing.
    */
  private val moved = "￼"

  test(
    "Two neighbours moved before or after a word, whichever is moved first, leave no space before a stop.",
  ):
    assertEquals(
      placed(
        "what I have seen.",
        "have",
        Piece("evah", Before("what")),
      ),
      Some("evah what I seen."),
    )
    assertEquals(
      placed(
        s"${ moved }what I seen.",
        "seen",
        Piece("nees", Before("what")),
      ),
      Some(s"${ moved }nees what I."),
    )
    assertEquals(
      placed(
        "what I have seen.",
        "seen",
        Piece("nees", Before("what")),
      ),
      Some("nees what I have."),
    )
    assertEquals(
      placed(
        s"${ moved }what I have.",
        "have",
        Piece("evah", Before("what")),
      ),
      Some(s"${ moved }evah what I."),
    )
    assertEquals(
      placed(
        "what I have seen.",
        "have",
        Piece("evah", After("what")),
      ),
      Some("what evah I seen."),
    )
    assertEquals(
      placed(
        s"what$moved I seen.",
        "seen",
        Piece("nees", After("what")),
      ),
      Some(s"what nees$moved I."),
    )
