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

  /**
    * The words of `text` chosen at `intensity`, of those drawn up to `floor`
    * already, drawing each from `chances` in turn.
    */
  private def chosen
    (
      text: String,
      floor: Double,
      intensity: Double,
      chances: Double*,
    )
    : List[String] =
    val next = chances.iterator
    Words
      .choose(
        Words.find(text),
        floor,
        intensity,
        () => next.next(),
      )
      .map(_.span.in(text))

  /** As many words, each `word`, as `count`. */
  private def many(count: Int): List[Span] =
    Words.find(List.fill(count)("word").mkString(" "))

  test("A word is chosen when its draw is below the intensity."):
    assertEquals(
      chosen(
        "one two three four",
        0,
        0.05,
        0.9,
        0.01,
        0.5,
        0.04,
      ),
      List("two", "four"),
    )

  test("A word of one letter is never chosen."):
    assertEquals(chosen("I a x", 0, 1), Nil)

  test("No word is chosen at an intensity of 0."):
    assertEquals(chosen("one two", 0, 0, 0, 0), Nil)

  test("An intensity is kept from 0 up to the greatest that may be chosen."):
    assertEquals(
      List(-1, 0, 0.2, 0.4, 0.9).map(Words.bounded),
      List(0, 0, 0.2, 0.4, 0.4),
    )

  test("Words drawn up to a floor already are drawn again above it."):
    // From `0.2` up, `0.5` of the way is `0.6`, and `0.1` of the way is `0.28`.
    assertEquals(
      chosen("one two", 0.2, 0.4, 0.5, 0.1),
      List("two"),
    )

  test("About one word in twenty is chosen at first."):
    val chosen = Words
      .choose(
        many(10_000),
        0,
        Words.defaultIntensity,
        () => math.random(),
      )
      .size
    assert(chosen > 400 && chosen < 600, chosen)

  test("Raising the intensity in steps chooses as many words as at once."):
    val words = many(10_000)
    val first = Words.choose(words, 0, 0.1, () => math.random())
    val rest  = words.diff(first.map(_.span))
    val more  = Words.choose(rest, 0.1, 0.3, () => math.random())
    val all   = first.size + more.size
    assert(all > 2800 && all < 3200, all)
    assert(more.forall(word => word.draw >= 0.1 && word.draw < 0.3))

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

  test("Items are packed into batches by their weights, each kept whole."):
    assertEquals(
      Words.weighed(List(2, 1, 3, 1), 3)(identity),
      List(List(2, 1), List(3), List(1)),
    )

  /** The text of each hint placed in `text`, with how it is shown. */
  private def placed(text: String, hints: Hint*): List[(String, Look)] = Words
    .place(text, hints.toList)
    .map((span, look) => span.in(text) -> look)

  private def explained(original: String, meaning: String) =
    Hint(original, meaning, Hint.Kind.Explained)

  private def glossed(original: String, meaning: String) =
    Hint(original, meaning, Hint.Kind.Glossed)

  test("A hint is found as a whole word, never within another."):
    assertEquals(
      Words.place(
        "Die Tante ruft an.",
        List(explained("an", "on")),
      ),
      List(Span(15, 17) -> Look("an", "on", glossed = false)),
    )

  test("Each hint is found after the one before it."):
    assertEquals(
      Words
        .place(
          "Haus und Haus",
          List(
            explained("Haus", "house"),
            explained("Haus", "home"),
          ),
        )
        .map(_._1),
      List(Span(0, 4), Span(9, 13)),
    )

  test("A phrase is found however its words are spaced."):
    assertEquals(
      placed(
        "Das geht mir auf  den\n Keks.",
        glossed("auf den Keks", "annoying"),
      ).map(_._1),
      List("auf  den\n Keks"),
    )

  test(
    "Each part of a phrase split by other words is explained, never replaced.",
  ):
    assertEquals(
      placed(
        "Er ruft seine Mutter an.",
        glossed("ruft … an", "calls"),
      ),
      List(
        "ruft" -> Look("ruft", "calls", glossed = false),
        "an"   -> Look("an", "calls", glossed = false),
      ),
    )

  test("A hint not found, or that would overlap one before, is left out."):
    assertEquals(
      placed(
        "Er ruft seine Mutter an.",
        explained("Zebra", "zebra"),
        explained("ruft ... an", "calls"),
        explained("an", "on"),
      ).map(_._1),
      List("ruft", "an"),
    )

  test(
    "A glossed word is shown as its meaning, capitalised only where it begins a sentence.",
  ):
    assertEquals(
      placed(
        "Ausschreibungen sind selten. Er hat die Ausschreibung gewonnen.",
        glossed("Ausschreibungen", "tenders"),
        explained("selten", "rare"),
        glossed("Ausschreibung", "tender"),
      ),
      List(
        "Ausschreibungen" -> Look(
          "Tenders",
          "Ausschreibungen",
          glossed = true,
        ),
        "selten"        -> Look("selten", "rare", glossed = false),
        "Ausschreibung" -> Look("tender", "Ausschreibung", glossed = true),
      ),
    )
