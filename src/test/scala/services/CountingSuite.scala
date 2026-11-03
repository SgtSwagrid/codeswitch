package com.alecdorrington.codeswitch.services

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.alecdorrington.codeswitch.lexicon.{
  Dictionary, Pending, Spelling, Store,
}
import com.alecdorrington.codeswitch.models.{Hint, Kaikki, Lookup, Wiktionary}
import com.alecdorrington.iris.{Model, ModelClient}
import com.alecdorrington.megalingual.Language
import munit.CatsEffectSuite
import scala.concurrent.duration.DurationInt

class CountingSuite extends CatsEffectSuite:

  private val german  = Language.German
  private val spanish = Language.Spanish

  /** Kaikki's files, by the word of `language` they are of. */
  private val files: Map[(Language, String), String] = Map(
    (german, "Bank")  -> Kaikki.lines("Bank"),
    (german, "gehen") ->
      """{"word": "gehen", "pos": "verb", "senses": [{"glosses": ["to go"]}], "forms": [{"form": "ging", "tags": ["preterite"]}]}""",
    (german, "ging") ->
      """{"word": "ging", "pos": "verb", "senses": [{"glosses": ["first/third-person singular preterite of gehen"], "tags": ["form-of", "preterite"], "form_of": [{"word": "gehen"}]}]}""",
    (german, "die") ->
      """{"word": "die", "pos": "article", "senses": [{"glosses": ["the"]}]}""",
    (spanish, "venir") ->
      """{"word": "venir", "pos": "verb", "senses": [{"glosses": ["to come"]}], "forms": [{"form": "vino", "tags": ["preterite", "third-person", "singular"]}]}""",
    (spanish, "vino") ->
      """{"word": "vino", "pos": "noun", "senses": [{"glosses": ["wine"]}]}
        |{"word": "vino", "pos": "verb", "senses": [{"glosses": ["third-person singular preterite of venir"], "tags": ["form-of"], "form_of": [{"word": "venir"}]}]}"""
        .stripMargin,
  )

  /** Kaikki, with `files`, recording each address asked for. */
  private def kaikki(asked: Ref[IO, List[String]]): Lexicon.Fetch = url =>
    asked
      .update(_ :+ url)
      .as(files.collectFirst {
        case ((language, word), lines)
          if Wiktionary.url(language, word) == url => lines
      })

  /** The words of `language` asked for of Kaikki, as their addresses name them. */
  private def words(language: Language, asked: List[String]): List[String] =
    asked.flatMap(url =>
      files
        .keys
        .collect {
          case (each, word)
            if each == language && Wiktionary.url(each, word) == url => word
        },
    )

  /** The browser's storage and a lexicon, both in memory, and its counts. */
  private def fresh: IO[(Memory, Store, FrequencyService)] =
    (Memory(_ => false, 0.millis), Store.memory).mapN((storage, store) =>
      (storage, store, FrequencyService(storage)),
    )

  private def hint(original: String, word: String) = Hint(
    original,
    "meaning",
    "meaning",
    Hint.Kind.Explained,
    word,
    german,
  )

  test(
    "A passage's words are counted toward the dictionary forms that the lexicon lists their spellings as forms of, a form of two words alike, as Bank, toward their one form, and a hinted word toward the form its hint names.",
  ):
    for
      (storage, store, frequencies) <- fresh
      asked                         <- Ref.of[IO, List[String]](Nil)
      _ <- Lexicon.learn(german, "Bank", store, kaikki(asked))
      _ <- Lexicon.learn(german, "gehen", store, kaikki(asked))
      _ <- Counting.read(
        german,
        "Er ging zur Bank, und die Bank war zu.",
        List(hint("ging", "gehen")),
        store,
        frequencies,
      )
      counts  <- frequencies.of(german, List("gehen", "Bank"))
      waiting <- store.waiting
    yield
      assertEquals(counts, Map("gehen" -> 1.0, "Bank" -> 2.0))
      assertEquals(
        waiting.map(_.spelling.text).toSet,
        Set("Er", "zur", "und", "die", "war", "zu"),
      )
      assert(
        waiting.find(_.spelling.text == "Er").exists(_.opening),
        waiting,
      )

  test(
    "Spellings the lexicon knows nothing of wait, and are looked up one at a time, then counted toward what they are found to be: a form of a word as that word, one that begins a sentence as begun in lower case, and one no dictionary has not at all.",
  ):
    for
      (storage, store, frequencies) <- fresh
      asked                         <- Ref.of[IO, List[String]](Nil)
      _                             <- Counting.read(
        german,
        "Ging sie? Die Frau ging.",
        Nil,
        store,
        frequencies,
      )
      before <- frequencies.of(german, List("gehen", "die"))
      _      <- Counting.look(
        store,
        storage,
        kaikki(asked),
        frequencies,
      )
      after <- frequencies.of(
        german,
        List("gehen", "die", "Frau", "sie"),
      )
      left <- store.waiting
      urls <- asked.get
    yield
      assertEquals(before, Map("gehen" -> 0.0, "die" -> 0.0))
      assertEquals(
        after,
        Map(
          "gehen" -> 2.0,
          "die"   -> 1.0,
          "Frau"  -> 0.0,
          "sie"   -> 0.0,
        ),
      )
      assertEquals(left, Nil)
      assertEquals(
        words(german, urls).distinct,
        List("ging", "gehen", "die"),
      )

  test(
    "A spelling that the lexicon lists only from another word's table is looked up before it is counted, as it may be a word of its own too, then shared evenly among all it is.",
  ):
    for
      (storage, store, frequencies) <- fresh
      asked                         <- Ref.of[IO, List[String]](Nil)
      _ <- Lexicon.learn(spanish, "venir", store, kaikki(asked))
      _ <- Counting.read(
        spanish,
        "el vino",
        Nil,
        store,
        frequencies,
      )
      before <- frequencies.of(spanish, List("venir", "vino"))
      _      <- Counting.look(
        store,
        storage,
        kaikki(asked),
        frequencies,
      )
      after <- frequencies.of(spanish, List("venir", "vino"))
    yield
      assertEquals(before, Map("venir" -> 0.0, "vino" -> 0.0))
      assertEquals(after, Map("venir" -> 0.5, "vino" -> 0.5))

  test(
    "With Wiktionary turned off, nothing waiting is looked up, and it waits on.",
  ):
    for
      (storage, store, frequencies) <- fresh
      asked                         <- Ref.of[IO, List[String]](Nil)
      _ <- Counting.read(german, "Weg", Nil, store, frequencies)
      _ <- storage.set(
        Lexicon.off,
        Set(Dictionary.Wiktionary),
      )
      _ <- Counting.look(
        store,
        storage,
        kaikki(asked),
        frequencies,
      )
      left <- store.waiting
      urls <- asked.get
    yield
      assertEquals(urls, Nil)
      assertEquals(left.map(_.spelling.text), List("Weg"))

  test(
    "A lookup that fails stops the looking up, and its spelling waits on behind the others, lest one that always fails keep them all waiting; a spelling read again while it waits is counted as often.",
  ):
    for
      (storage, store, frequencies) <- fresh
      _ <- Counting.read(german, "Weg", Nil, store, frequencies)
      _ <- IO.sleep(5.millis)
      _ <- Counting.read(
        german,
        "Ufer Weg",
        Nil,
        store,
        frequencies,
      )
      _ <- Counting.look(
        store,
        storage,
        _ => IO.raiseError(Problem("Wiktionary could not be reached.")),
        frequencies,
      )
      left <- store.waiting
    yield assertEquals(
      left.map(waiting => waiting.spelling.text -> waiting.count),
      List("Ufer" -> 1.0, "Weg" -> 2.0),
    )

  test(
    "Each waiting spelling is kept by its whole language and spelling, so that its varieties share it.",
  ):
    assertEquals(
      Pending.key(
        Language.SwissStandardGerman,
        Spelling(german, "Weg"),
      ),
      "de:Weg",
    )

  test(
    "A word looked up as one of its forms, as `ging`, is shown as the word it is a form of.",
  ):
    for
      (storage, store, _) <- fresh
      asked               <- Ref.of[IO, List[String]](Nil)
      found               <- Lexicon.entry(
        Language.BritishEnglish,
        Lookup("ging", "verb", "went", german),
        store,
        storage,
        kaikki(asked),
        [A] =>
          (_: (Model, ModelClient[IO]) => IO[A]) =>
            IO.raiseError(Problem("No key.")),
      )
    yield assertEquals(
      found.map(entry => (entry.word, entry.senses.head.glosses)),
      Some(("gehen", List("to go"))),
    )
