package com.alecdorrington.codeswitch

import com.alecdorrington.annotatedtextformat.TextSpan
import com.alecdorrington.annotatedtextformat.dom.Spacing
import com.alecdorrington.codeswitch.Place.*
import munit.FunSuite

class PlacementSuite extends FunSuite:

  /**
    * The text that `pieces` of the translation of `word` make of `text`, all of
    * which is one text node, or nothing if they are shown nowhere.
    */
  private def placed
    (text: String, word: String, pieces: Piece*)
    : Option[String] = lay(
    text,
    TextSpan(0, text.length),
    word,
    pieces*,
  ).map(render(text, _))

  private def lay
    (
      text: String,
      node: TextSpan,
      word: String,
      pieces: Piece*,
    )
    : Option[Layout] =
    val start = text.indexOf(word)
    Placement.lay(
      text,
      node,
      TextSpan(start, start + word.length),
      Translation(pieces.toList),
    )

  /**
    * `text`, with what `layout` shows in it, as Annotated Text Format puts it
    * there: from the end back, each point's pieces spaced off the words beside
    * it and put before what begins there, and the space beside an emptied home
    * taken away with it.
    */
  private def render(text: String, layout: Layout): String =
    (layout
      .parts
      .map((span, shown) =>
        span ->
          (if span.isPoint then spaced(text, span.start, shown) else shown),
      ) ++
      Option
        .when(layout.shown.isEmpty)(Spacing.beside(text, layout.home))
        .flatten
        .map(_ -> ""))
      .sortBy((span, _) => (-span.start, span.isPoint))
      .foldLeft(text):
        case (text, (span, shown)) => text.patch(span.start, shown, span.length)

  /** `piece`, put in `text` at `at`, with the spaces that it needs there. */
  private def spaced(text: String, at: Int, piece: String): String =
    val (before, after) = Spacing.around(text, at)
    s"${ if before then " " else "" }$piece${ if after then " " else "" }"

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
        TextSpan(4, 9),
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
        TextSpan(14, 20),
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
        "evah what I seen.",
        "seen",
        Piece("nees", Before("what")),
      ),
      Some("evah nees what I."),
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
        "nees what I have.",
        "have",
        Piece("evah", Before("what")),
      ),
      Some("nees evah what I."),
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
        "what evah I seen.",
        "seen",
        Piece("nees", After("what")),
      ),
      Some("what nees evah I."),
    )
