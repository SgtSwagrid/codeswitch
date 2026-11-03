package com.alecdorrington.codeswitch.services

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.alecdorrington.codeswitch.lexicon.{
  Dictionary, Page, Pending, Source, Spelling, Store, Timestamp,
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
    (german, "gewinnen") -> Kaikki.lines("gewinnen"),
    (german, "haben")    ->
      """{"word": "haben", "pos": "verb", "senses": [{"glosses": ["to have"]}], "forms": [{"form": "hat", "tags": ["present", "singular", "third-person"]}]}""",
    (german, "laufen") ->
      """{"word": "laufen", "pos": "verb", "senses": [{"glosses": ["to run"]}]}""",
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

  private def hint
    (
      original: String,
      word: String,
      wordClass: String = "",
    ) = Hint(
    original,
    "meaning",
    "meaning",
    Hint.Kind.Explained,
    word,
    german,
    wordClass,
  )

  /** The model that wrote the hints, reading them on a page. */
  private val hinting = Source.Generated(
    Model.ClaudeHaiku4_5,
    Some(Page("https://example.org/", "Example")),
    Timestamp(0),
    Nil,
  )

  /** The forms that `store` keeps of the German word `word`'s lexemes. */
  private def formsOf(store: Store, word: String) = store
    .homographs(List(Spelling(german, word)))
    .flatMap(listed => store.lexemes(listed.head.lexemes))
    .map(_.filter(_.lemma.spelling.text == word).flatMap(_.forms))

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
    "A spelling read again while its lookup fails is counted as often, as the lookup moves it behind the others.",
  ):
    for
      (storage, store, frequencies) <- fresh
      _ <- Counting.read(german, "Weg", Nil, store, frequencies)
      _ <- Counting.look(
        store,
        storage,
        _ =>
          Counting.read(german, "Weg", Nil, store, frequencies) *>
            IO.raiseError(Problem("Wiktionary could not be reached.")),
        frequencies,
      )
      left <- store.waiting
    yield assertEquals(
      left.map(waiting => waiting.spelling.text -> waiting.count),
      List("Weg" -> 2.0),
    )

  test(
    "A verb's auxiliary is no form of it: `haben`, read with `gewinnen` known, is counted toward `haben` alone, once looked up.",
  ):
    for
      (storage, store, frequencies) <- fresh
      asked                         <- Ref.of[IO, List[String]](Nil)
      _      <- Lexicon.learn(german, "gewinnen", store, kaikki(asked))
      listed <- store.homographs(List(Spelling(german, "haben")))
      _      <- Counting.read(
        german,
        "Sie haben gewonnen.",
        Nil,
        store,
        frequencies,
      )
      before <- frequencies.of(german, List("gewinnen", "haben"))
      _      <- Counting.look(
        store,
        storage,
        kaikki(asked),
        frequencies,
      )
      after <- frequencies.of(german, List("gewinnen", "haben"))
    yield
      assertEquals(listed.flatMap(_.lexemes), Nil)
      assertEquals(
        before,
        Map("gewinnen" -> 0.0, "haben" -> 0.0),
      )
      assertEquals(
        after,
        Map("gewinnen" -> 1.0, "haben" -> 1.0),
      )

  test(
    "The words of a language written without spaces between them are found by the rules for it, each counted apart, as `猫` twice.",
  ):
    for
      (_, store, frequencies) <- fresh
      _                       <- Counting.read(
        Language.Japanese,
        "私は猫が好きです。猫がいます。",
        Nil,
        store,
        frequencies,
      )
      waiting <- store.waiting
    yield
      val read = waiting
        .map(waiting => waiting.spelling.text -> waiting.count)
        .toMap
      assertEquals(read.get("猫"), Some(2.0), read)
      assert(read.contains("好き"), read)
      assert(read.keys.forall(_.length <= 3), read)

  test(
    "A hint teaches the lexicon that the word it is of is a form of the word it names, credited to the model on its page, so that it is counted toward that word when read with no hint, though the dictionary lacks it.",
  ):
    for
      (storage, store, frequencies) <- fresh
      asked                         <- Ref.of[IO, List[String]](Nil)
      _ <- Lexicon.learn(german, "gehen", store, kaikki(asked))
      _ <- Counting.heed(
        german,
        List(hint("gehst", "gehen", "verb")),
        hinting,
        store,
      )
      forms <- formsOf(store, "gehen")
      _     <- Counting.read(
        german,
        "Du gehst.",
        Nil,
        store,
        frequencies,
      )
      _ <- Counting.look(
        store,
        storage,
        kaikki(asked),
        frequencies,
      )
      count <- frequencies.of(german, List("gehen"))
    yield
      assertEquals(
        forms.filter(_.spelling.text == "gehst").map(_.source),
        List(hinting),
      )
      assertEquals(count, Map("gehen" -> 1.0))

  test(
    "A word that hints name, not yet looked up, waits to be, counted no more for it, and once it is, lists the words they were of among its forms.",
  ):
    for
      (storage, store, frequencies) <- fresh
      asked                         <- Ref.of[IO, List[String]](Nil)
      _                             <- Counting.heed(
        german,
        List(
          hint("läuft", "laufen", "verb"),
          hint("lief", "laufen", "verb"),
        ),
        hinting,
        store,
      )
      waiting <- store.waiting
      _       <- Counting.look(
        store,
        storage,
        kaikki(asked),
        frequencies,
      )
      forms <- formsOf(store, "laufen")
      count <- frequencies.of(german, List("laufen"))
      left  <- store.waiting
    yield
      assertEquals(
        waiting.map(each => (each.spelling.text, each.count, each.hinted.size)),
        List(("laufen", 0.0, 2)),
      )
      assertEquals(
        forms.map(_.spelling.text).toSet,
        Set("läuft", "lief"),
      )
      assertEquals(count, Map("laufen" -> 0.0))
      assertEquals(left, Nil)

  test(
    "A hint teaches nothing of a word class its word has no lexeme of, of a phrase, of a form already listed, or with no word class.",
  ):
    for
      (_, store, _) <- fresh
      asked         <- Ref.of[IO, List[String]](Nil)
      _             <- Lexicon.learn(german, "gehen", store, kaikki(asked))
      before        <- formsOf(store, "gehen")
      _             <- Counting.heed(
        german,
        List(
          hint("gehst", "gehen", "noun"),
          hint("geht … weg", "weggehen", "verb"),
          hint("ging", "gehen", "verb"),
          hint("gingen", "gehen"),
        ),
        hinting,
        store,
      )
      after   <- formsOf(store, "gehen")
      waiting <- store.waiting
    yield
      assertEquals(after, before)
      assertEquals(waiting, Nil)

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
      found.map((_, entry) => (entry.word, entry.senses.head.glosses)),
      Some(("gehen", List("to go"))),
    )
