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

  test("A word's context reaches only so far on each side."):
    assertEquals(
      Words.context("a" * 200 + " ", "word", " " + "b" * 200),
      "a" * 99 + " [[word]] " + "b" * 99,
    )

  test("Spacing is collapsed before a context is cut to length."):
    assertEquals(
      Words.context("a" * 98 + " " * 50, "word", ""),
      "a" * 98 + " [[word]]",
    )

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
