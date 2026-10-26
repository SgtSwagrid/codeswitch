package com.alecdorrington.codeswitch

import munit.FunSuite

class WordsSuite extends FunSuite:

  private def words(text: String): List[String] = Words
    .find(text)
    .map(_.in(text))

  test("Words are runs of letters, in any script."):
    assertEquals(
      words("Hello, wörld! Привет 42 times."),
      List("Hello", "wörld", "Привет", "times"),
    )

  test("An apostrophe or hyphen within a word belongs to it."):
    assertEquals(
      words("It's a well-known 'quote' - isn't it?"),
      List(
        "It's",
        "a",
        "well-known",
        "quote",
        "isn't",
        "it",
      ),
    )

  test("A word is chosen when its chance comes up."):
    val chances = Iterator(0.9, 0.01, 0.5, 0.04)
    val spans   = Words.find("one two three four")
    assertEquals(
      Words.choose(spans, () => chances.next()).map(_.in("one two three four")),
      List("two", "four"),
    )

  test("A word of one letter is never chosen."):
    assertEquals(
      Words.choose(Words.find("I a x"), () => 0.0),
      Nil,
    )

  test("About one word in twenty is chosen."):
    val text   = List.fill(10_000)("word").mkString(" ")
    val chosen = Words.choose(Words.find(text), () => math.random()).size
    assert(chosen > 400 && chosen < 600, chosen)

  test("A word's context marks it, between the texts around it."):
    assertEquals(
      Words.context(
        "\n  Once,  the ",
        "cat",
        " sat\n on the mat.  ",
      ),
      "Once, the [[cat]] sat on the mat.",
    )

  test("A word's context takes in the rest of its sentence."):
    assertEquals(
      Words.context(
        "Before. The " + "very " * 40,
        "cat",
        " " + "lazily " * 40 + "sat. After.",
      ),
      "…The " + "very " * 40 + "[[cat]] " + "lazily " * 40 + "sat. …",
    )

  test("A word's context reaches the usual distance beyond its sentence."):
    assertEquals(
      Words.context("One. Two. ", "three", ". Four."),
      "One. Two. [[three]]. Four.",
    )

  test("A word's context reaches only so far on each side, marked where cut."):
    assertEquals(
      Words.context("a" * 400 + " ", "word", " " + "b" * 400),
      "…" + "a" * 299 + " [[word]] " + "b" * 299 + "…",
    )

  test("Spacing is collapsed before a context is cut to length."):
    assertEquals(
      Words.context("a" * 98 + " " * 50, "word", ""),
      "a" * 98 + " [[word]]",
    )

  test("A word's sentence is found."):
    assertEquals(
      Words.sentenceAt("It rained. Then it stopped.", 12),
      Span(11, 27),
    )

  test("A line break in the page's text does not end a sentence."):
    assertEquals(
      Words.sentenceAt("It was\nraining. Then", 0),
      Span(0, 16),
    )

  test("Where the browser cannot find sentences, each ends after its stop."):
    assertEquals(
      Words.byStops("It rained. Then it stopped. Done"),
      List(Span(0, 11), Span(11, 28), Span(28, 32)),
    )
    assertEquals(
      Words.byStops("He said “no.” Then left."),
      List(Span(0, 14), Span(14, 24)),
    )

  test(
    "A quote is found whole, with either apostrophe, never in a longer word.",
  ):
    val text = "It’s a well-known fact."
    assertEquals(
      Words.occurrences(text, "it's"),
      List(Span(0, 4)),
    )
    assertEquals(
      Words.occurrences(text, "well-known"),
      List(Span(7, 17)),
    )
    assertEquals(Words.occurrences(text, "well"), Nil)
    assertEquals(Words.occurrences(text, "It"), Nil)

  test("A quote is found as whole words, in any case and spacing."):
    val text = "The other theme. the\n  end"
    assertEquals(
      Words.occurrences(text, "the"),
      List(Span(0, 3), Span(17, 20)),
    )
    assertEquals(
      Words.occurrences(text, "THE end"),
      List(Span(17, 26)),
    )
    assertEquals(Words.occurrences(text, " "), Nil)

  test("A translation begins with a capital where its original does."):
    assertEquals(
      Words.matchCase("House", "maison"),
      "Maison",
    )
    assertEquals(
      Words.matchCase("house", "Haus"),
      "Haus",
    )
    assertEquals(
      Words.matchCase("house", "maison"),
      "maison",
    )

  test("Groups are packed into batches, each kept whole."):
    assertEquals(
      Words.batches(
        List(List(1, 2), List(3), List(4, 5, 6), List(7)),
        3,
      ),
      List(List(1, 2, 3), List(4, 5, 6), List(7)),
    )

  test("A group larger than a batch is a batch of its own."):
    assertEquals(
      Words.batches(List(List(1), List(2, 3, 4), List(5)), 2),
      List(List(1), List(2, 3, 4), List(5)),
    )
