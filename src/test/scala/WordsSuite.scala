package com.alecdorrington.codeswitch

import com.alecdorrington.annotatedtextformat.{Annotation, Span}
import munit.FunSuite

class WordsSuite extends FunSuite:

  private def words(text: String): List[String] = Words
    .find(text)
    .map(_.in(text))

  test(
    "Words are runs of letters, in any script, and an apostrophe or hyphen within a word belongs to it.",
  ):
    assertEquals(
      words("Hello, wörld! Привет 42 times."),
      List("Hello", "wörld", "Привет", "times"),
    )
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

  test(
    "A word of more than one letter is chosen when its draw, above any floor it was drawn up to already, is below the intensity, which is at first 0.05, and is kept from 0 up to the greatest that may be chosen.",
  ):
    assertEqualsDouble(Words.defaultIntensity, 0.05, 1e-9)
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
    assertEquals(chosen("I a x", 0, 1), Nil)
    assertEquals(chosen("one two", 0, 0, 0, 0), Nil)
    assertEquals(
      List(-1, 0, 0.2, 0.4, 0.9).map(Words.bounded),
      List(0, 0, 0.2, 0.4, 0.4),
    )
    // From `0.2` up, `0.5` of the way is `0.6`, and `0.1` of the way is `0.28`.
    assertEquals(
      chosen("one two", 0.2, 0.4, 0.5, 0.1),
      List("two"),
    )
    val draws = Iterator(0.5, 0.1)
    assertEqualsDouble(
      Words
        .choose(
          Words.find("one two"),
          0.2,
          0.4,
          () => draws.next(),
        )
        .head
        .draw,
      0.28,
      1e-9,
    )

  test(
    "A word's context marks it, between the texts around it, its spacing collapsed before it is cut to length.",
  ):
    assertEquals(
      Words.context(
        "\n  Once,  the ",
        "cat",
        " sat\n on the mat.  ",
      ),
      "Once, the [[cat]] sat on the mat.",
    )
    assertEquals(
      Words.context("a" * 98 + " " * 50, "word", ""),
      "a" * 98 + " [[word]]",
    )

  test(
    "A word's context takes in the rest of its sentence, or else the usual distance beyond it, but reaches only so far on each side, marked where cut.",
  ):
    assertEquals(
      Words.context(
        "Before. The " + "very " * 40,
        "cat",
        " " + "lazily " * 40 + "sat. After.",
      ),
      "…The " + "very " * 40 + "[[cat]] " + "lazily " * 40 + "sat. …",
    )
    assertEquals(
      Words.context("One. Two. ", "three", ". Four."),
      "One. Two. [[three]]. Four.",
    )
    assertEquals(
      Words.context("a" * 400 + " ", "word", " " + "b" * 400),
      "…" + "a" * 299 + " [[word]] " + "b" * 299 + "…",
    )

  test(
    "A word's sentence is found, which a line break in the page's text does not end.",
  ):
    assertEquals(
      Words.sentenceAt("It rained. Then it stopped.", 12),
      Span(11, 27),
    )
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
    "A quote is found whole, as whole words, with either apostrophe, in any case and spacing, never in a longer word.",
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
    val spaced = "The other theme. the\n  end"
    assertEquals(
      Words.occurrences(spaced, "the"),
      List(Span(0, 3), Span(17, 20)),
    )
    assertEquals(
      Words.occurrences(spaced, "THE end"),
      List(Span(17, 26)),
    )
    assertEquals(Words.occurrences(spaced, " "), Nil)

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

  test(
    "Groups are packed into batches, each kept whole, one larger than a batch as a batch of its own, and items likewise by their weights.",
  ):
    assertEquals(
      Words.batches(
        List(List(1, 2), List(3), List(4, 5, 6), List(7)),
        3,
      ),
      List(List(1, 2, 3), List(4, 5, 6), List(7)),
    )
    assertEquals(
      Words.batches(List(List(1), List(2, 3, 4), List(5)), 2),
      List(List(1), List(2, 3, 4), List(5)),
    )
    assertEquals(
      Words.weighed(List(2, 1, 3, 1), 3)(identity),
      List(List(2, 1), List(3), List(1)),
    )

  /** The text of each part of each hint placed in `text`, with how it is shown. */
  private def placed(text: String, hints: Hint*): List[(String, Marking)] =
    Words
      .place(text, hints.toList)
      .flatMap(annotation =>
        annotation.spans.map(_.in(text) -> annotation.value),
      )

  private def explained(original: String, meaning: String) = Hint(
    original,
    meaning,
    Hint.Kind.Explained,
    original,
  )

  private def glossed(original: String, meaning: String) = Hint(
    original,
    meaning,
    Hint.Kind.Glossed,
    original,
  )

  test("A hint is found as a whole word, never within another."):
    assertEquals(
      Words.place(
        "Die Tante ruft an.",
        List(explained("an", "on")),
      ),
      List(Annotation(
        Span(15, 17),
        Marking(
          None,
          "on",
          Hint.Kind.Explained,
          explained("an", "on"),
        ),
      )),
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
        .flatMap(_.spans),
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
        "ruft" -> Marking(
          None,
          "calls",
          Hint.Kind.Explained,
          glossed("ruft … an", "calls"),
        ),
        "an" -> Marking(
          None,
          "calls",
          Hint.Kind.Explained,
          glossed("ruft … an", "calls"),
        ),
      ),
    )

  test(
    "A hint given out of order is placed if its words are in the text only once.",
  ):
    assertEquals(
      placed(
        "Er ruft seine Mutter an. Sie kommt morgen.",
        explained("kommt", "comes"),
        explained("Mutter", "mother"),
      ).map(_._1),
      List("kommt", "Mutter"),
    )

  test(
    "A hint never matches part of a word joined to another by a hyphen or an apostrophe.",
  ):
    assertEquals(
      placed(
        "It is well-known.",
        explained("well", "gut"),
      ),
      Nil,
    )
    assertEquals(
      placed(
        "I don’t know.",
        explained("dont", "nicht"),
      ),
      Nil,
    )
    assertEquals(
      placed(
        "I don’t know.",
        explained("don't", "nicht"),
      ).map(_._1),
      List("don’t"),
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
        "Ausschreibungen" -> Marking(
          Some("Tenders"),
          "Ausschreibungen",
          Hint.Kind.Glossed,
          glossed("Ausschreibungen", "tenders"),
        ),
        "selten" -> Marking(
          None,
          "rare",
          Hint.Kind.Explained,
          explained("selten", "rare"),
        ),
        "Ausschreibung" -> Marking(
          Some("tender"),
          "Ausschreibung",
          Hint.Kind.Glossed,
          glossed("Ausschreibung", "tender"),
        ),
      ),
    )

  test("A text is cut into its sentences, which cover it all."):
    val text = "Er kam. Er sah! Er siegte"
    assertEquals(
      Words.sentences(text).map(_.in(text)),
      List("Er kam. ", "Er sah! ", "Er siegte"),
    )

  test("A hint goes on a word in its own case before one in another."):
    assertEquals(
      placed(
        "Er ist weg. Der Weg ist lang.",
        explained("Weg", "path"),
      ).map(_._1),
      List("Weg"),
    )
    assertEquals(
      placed("Weg damit!", explained("weg", "away")).map(_._1),
      List("Weg"),
    )

  test(
    "A text is cut into excerpts of whole sentences, and a long sentence between its words.",
  ):
    val text = "Eins zwei. Drei vier fünf sechs sieben. Acht."
    assertEquals(
      Words.excerpts(text, 3).map(_.in(text)),
      List(
        "Eins zwei. ",
        "Drei vier fünf ",
        "sechs sieben. Acht.",
      ),
    )

  test(
    "A word that the reader knows is shown as it is, with its meaning on hovering.",
  ):
    val known = Hint("Weg", "path", Hint.Kind.Known, "Weg")
    assertEquals(
      placed("Der Weg ist lang.", known),
      List("Weg" -> Marking(None, "path", Hint.Kind.Known, known)),
    )
