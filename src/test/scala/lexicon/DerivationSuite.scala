package com.alecdorrington.codeswitch.lexicon

import com.alecdorrington.codeswitch.models
import com.alecdorrington.codeswitch.models.{Entry, Kaikki, Wiktionary}
import com.alecdorrington.iris.Model
import com.alecdorrington.megalingual.Language
import munit.FunSuite

class DerivationSuite extends FunSuite:

  /** Kaikki's files of words, kept whole for tests. */
  private val files = List(
    "Bank",
    "anrufen",
    "dom",
    "ev",
    "falar",
    "ganar",
    "gewinnen",
    "govorit",
    "kot",
    "manger",
    "mangiare",
    "puhua",
    "schnell",
    "talo",
    "winnen",
  )

  private val german  = Language.German
  private val spanish = Language.EuropeanSpanish

  /** As Wiktionary's record of `name` gave it, at the start of time. */
  private def sourced(name: String): Source.Sourced = Source.Sourced(
    Dictionary.Wiktionary,
    s"https://kaikki.org/$name",
    Timestamp(0),
  )

  /** The lexemes of Kaikki's file of `name`. */
  private def lexemes(name: String): List[Lexeme] = Derivation.fromKaikki(
    Kaikki.language(name),
    Kaikki.lines(name),
    sourced(name),
  )

  /** Everything shown. */
  private val all = (_: Source) => true

  /** `entry`, each of its forms' tags in order, as a set has none. */
  private def sorted(entry: Entry): Entry = entry.copy(forms =
    entry.forms.map(form => form.copy(tags = form.tags.sorted)),
  )

  test(
    "The entry of each lexeme of a word, shown to a reader of English, is what Wiktionary's entry was, in every file kept.",
  ):
    files.foreach(name =>
      assertEquals(
        lexemes(name).map(lexeme =>
          sorted(Derivation.entry(
            lexeme,
            Kaikki.language(name),
            Language.English,
            all,
          )),
        ),
        Kaikki.entries(name).map(sorted),
        name,
      ),
    )

  test(
    "Each of a word's entries is a lexeme of its own, by its etymology, with its gender, the words derived from it, how it is said, and its senses' synonyms.",
  ):
    val banks = lexemes("Bank")
    assertEquals(
      banks.map(_.id.text),
      List("de:noun:1:Bank", "de:noun:2:Bank"),
    )
    assertEquals(
      banks.map(_.properties.map(_.tag.name)),
      List(List("feminine"), List("feminine")),
    )
    assert(
      banks
        .flatMap(_.relations)
        .exists(relation =>
          relation.kind == Morphological.Derived &&
          relation.spelling == Spelling(german, "Bänkelsänger"),
        ),
    )
    assert(banks.head.lemma.pronunciations.exists(_.ipa.nonEmpty))
    assert(banks.head.lemma.pronunciations.exists(_.recording.nonEmpty))
    assert(
      files.flatMap(lexemes).flatMap(_.senses).exists(_.relations.nonEmpty),
    )
    assert(banks.flatMap(_.claims).forall(_.source == sourced("Bank")))

  test(
    "A form is spelt as its language writes it, without the marks of stress that a dictionary adds, which it keeps as how it is said, and one spelling may be several forms.",
  ):
    val house  = lexemes("dom").head
    val marked = house.forms.filter(_.pronunciations.exists(_.marked.nonEmpty))
    assert(marked.nonEmpty)
    assert(
      house.forms.forall(form => !form.spelling.text.exists(Set('̀', '́'))),
    )
    assertEquals(
      marked
        .flatMap(_.pronunciations.flatMap(_.marked))
        .filter(
          Spelling(Language.Russian, _) == Spelling(Language.Russian, "дома"),
        )
        .toSet,
      Set("до́ма", "дома́"),
    )
    assertEquals(
      Spelling(Language.Russian, "до́ма").text,
      "дома",
    )
    assertEquals(
      Spelling(Language.Spanish, " está ").text,
      "está",
    )
    assertEquals(
      Spelling(Language.Bulgarian, "къ́ща").text,
      "къща",
    )
    assertEquals(
      Spelling(Language.Bulgarian, "ѝ").text,
      "ѝ",
    )
    assertEquals(
      Spelling(
        Language.Russian,
        "сѐльскохозя́йственный",
      ).text,
      "сельскохозяйственный",
    )

  test(
    "Merged with what was kept of it, a lexeme fetched again has its source's claims anew, keeps each translation of a text still given alike, but not of one changed, and keeps what a model generated.",
  ):
    val kept       = lexemes("Bank").head
    val translated = Derivation.translated(
      kept,
      Derivation
        .entry(kept, german, Language.English, all)
        .copy(senses =
          Derivation
            .english(kept, german, all)
            .senses
            .map(sense =>
              sense.copy(glosses = sense.glosses.map(_.toUpperCase)),
            ),
        ),
      spanish,
      Model.ClaudeHaiku4_5,
      Timestamp(1),
      all,
    )
    val generated = Sense(
      SenseId(kept.id, "model", 0),
      Set(german),
      Set.empty,
      List(Explanation.of(
        spanish,
        "un banco",
        Source.Generated(
          Model.ClaudeHaiku4_5,
          None,
          Timestamp(2),
          Nil,
        ),
      )),
      Nil,
      Nil,
      Source.Generated(
        Model.ClaudeHaiku4_5,
        None,
        Timestamp(2),
        Nil,
      ),
    )
    val first   = kept.senses.head.definitions.head.versions.head.text
    val changed = kept.copy(senses =
      kept
        .senses
        .head
        .copy(definitions =
          Explanation.of(
            Language.English,
            "a changed gloss",
            sourced("Bank"),
          ) :: kept.senses.head.definitions.tail,
        ) :: kept.senses.tail,
    )
    val merged = Derivation.merged(
      Some(translated.copy(senses = translated.senses :+ generated)),
      changed,
    )
    val spanishOf = (lexeme: Lexeme) =>
      lexeme
        .senses
        .flatMap(_.definitions)
        .flatMap(_.versions)
        .filter(_.language == spanish)
        .map(_.text)
    assert(
      !spanishOf(merged).contains(first.toUpperCase),
      spanishOf(merged),
    )
    assertEquals(
      spanishOf(merged).filterNot(_ == "un banco"),
      spanishOf(translated).filterNot(_ == first.toUpperCase),
    )
    assert(spanishOf(merged).contains("un banco"))
    assertEquals(
      merged.senses.count(_.source == sourced("Bank")),
      kept.senses.size,
    )

  test(
    "A lexeme's texts from a source are put into another language as versions translated from that source, until none is left so, and none generated by a model is.",
  ):
    val kept = lexemes("Bank").head
    assert(Derivation.untranslated(kept, spanish, all))
    val english = Derivation.english(kept, german, all)
    val done    = Derivation.translated(
      kept,
      english.copy(
        senses = english
          .senses
          .map(sense =>
            sense.copy(
              glosses = sense.glosses.map(_ + "!"),
              examples = sense.examples.map(_.copy(translation = Some("¡sí!"))),
            ),
          ),
        etymology = english.etymology.map(_ => "origen"),
      ),
      spanish,
      Model.ClaudeHaiku4_5,
      Timestamp(1),
      all,
    )
    assert(!Derivation.untranslated(done, spanish, all))
    val shown = Derivation.entry(done, german, spanish, all)
    assertEquals(
      shown.senses.map(_.glosses),
      Derivation
        .entry(kept, german, Language.English, all)
        .senses
        .map(_.glosses.map(_ + "!")),
    )
    assertEquals(shown.etymology, Some("origen"))
    assertEquals(
      shown.source,
      models
        .Source
        .Wiktionary(
          "https://kaikki.org/Bank",
          translated = true,
        ),
    )
    assertEquals(
      Derivation.entry(done, german, Language.English, all).source,
      models
        .Source
        .Wiktionary(
          "https://kaikki.org/Bank",
          translated = false,
        ),
    )
    assert(
      done
        .claims
        .collect {
          case version: Version if version.language == spanish => version
        }
        .forall(
          _.source match
            case Source.Translated(from, _, Model.ClaudeHaiku4_5, _) => from ==
                sourced("Bank")
            case _ => false,
        ),
    )

  test(
    "A reader of a language beneath another is shown its versions, or else the broader language's, but never a sibling's.",
  ):
    val source      = sourced("Bank")
    val explanation = Explanation(List(
      Version(Language.Portuguese, "geral", source),
      Version(
        Language.BrazilianPortuguese,
        "brasileiro",
        source,
      ),
    ))
    assertEquals(
      explanation.in(Language.BrazilianPortuguese).map(_.text),
      Some("brasileiro"),
    )
    assertEquals(
      explanation.in(Language.EuropeanPortuguese).map(_.text),
      Some("geral"),
    )
    assertEquals(
      Explanation
        .of(
          Language.BrazilianPortuguese,
          "brasileiro",
          source,
        )
        .in(Language.EuropeanPortuguese),
      None,
    )

  test(
    "Nothing that a dictionary turned off gave is shown, but what a model generated is.",
  ):
    val kept = lexemes("Bank").head
    val off  =
      (source: Source) => !source.dictionaries.contains(Dictionary.Wiktionary)
    val shown = Derivation.entry(kept, german, Language.English, off)
    assertEquals(
      (shown.senses, shown.forms, shown.genders, shown.ipa, shown.etymology),
      (Nil, Nil, Nil, None, None),
    )
    assertEquals(shown.source, models.Source.Model)

  test(
    "An entry whose every sense is a form or another spelling of another word is no lexeme, but points to that word, as `ging` to `gehen`, and an entry with a meaning of its own too keeps that, as `vino`, wine, does; a shortened sense, as `talo`'s ellipsis of `taloyhtiö`, is a meaning of its own.",
  ):
    val ging =
      """{"word": "ging", "pos": "verb", "senses": [{"glosses": ["first/third-person singular preterite of gehen"], "tags": ["form-of", "preterite"], "form_of": [{"word": "gehen"}]}]}"""
    val vino = """{"word": "vino", "pos": "noun", "senses": [{"glosses": ["wine"]}]}
        |{"word": "vino", "pos": "verb", "senses": [{"glosses": ["third-person singular preterite of venir"], "tags": ["form-of"], "form_of": [{"word": "venir"}]}]}"""
      .stripMargin
    val strasse =
      """{"word": "Strasse", "pos": "noun", "senses": [{"glosses": ["Switzerland and Liechtenstein standard spelling of Straße"], "tags": ["Liechtenstein", "Switzerland", "alt-of", "alternative"], "alt_of": [{"word": "Straße"}]}]}"""
    val spanish = Language.Spanish
    assertEquals(
      Derivation.fromKaikki(german, ging, sourced("ging")),
      Nil,
    )
    assertEquals(
      Derivation.pointers(german, ging),
      List(WordClass("verb") -> Spelling(german, "gehen")),
    )
    assertEquals(
      Derivation
        .fromKaikki(spanish, vino, sourced("vino"))
        .map(lexeme => (lexeme.wordClass.name, lexeme.senses.size)),
      List("noun" -> 1),
    )
    assertEquals(
      Derivation.pointers(spanish, vino),
      List(WordClass("verb") -> Spelling(spanish, "venir")),
    )
    assertEquals(
      Derivation.pointers(german, strasse),
      List(WordClass("noun") -> Spelling(german, "Straße")),
    )
    assertEquals(
      Derivation.pointers(Language.Finnish, Kaikki.lines("talo")),
      Nil,
    )
    assert(
      lexemes("talo")
        .flatMap(_.senses.flatMap(_.definitions.flatMap(_.versions)))
        .exists(_.text.contains("taloyhtiö")),
    )

  /** As Wiktionary's record of `name` gave it when fetched again, at `at`. */
  private def again(name: String, at: Long): List[Lexeme] = Derivation
    .fromKaikki(
      Kaikki.language(name),
      Kaikki.lines(name),
      sourced(name).copy(at = Timestamp(at)),
    )

  /** `anrufen`, as Wiktionary gave it. */
  private val anrufen = lexemes("anrufen").head

  /** The first definition of the first sense of `lexeme`. */
  private def firstDefinition(lexeme: Lexeme): Piece = Piece.Definition(
    lexeme.senses.head.id,
    lexeme.senses.head.definitions.head,
    0,
  )

  /**
    * `anrufen`, its first definition reworded at `1`, then again at `2`, the
    * first example of its first sense deleted at `3`, its second sense deleted
    * at `4`, and its first form at `5`.
    */
  private val edited = (for
    once <- Editing
      .reworded(
        firstDefinition(anrufen),
        "to ring up",
        None,
        Language.English,
        Timestamp(1),
      )
      .flatMap(made =>
        Editing.replaced(
          anrufen,
          List(firstDefinition(anrufen)),
          List(made),
          Timestamp(1),
        ),
      )
    twice <- Editing
      .reworded(
        firstDefinition(once),
        "to telephone",
        None,
        Language.English,
        Timestamp(2),
      )
      .flatMap(made =>
        Editing.replaced(
          once,
          List(firstDefinition(once)),
          List(made),
          Timestamp(2),
        ),
      )
    unexampled <- Editing.deleted(
      twice,
      List(Piece.Usage(
        twice.senses.head.id,
        twice.senses.head.examples.head,
        0,
      )),
      Timestamp(3),
    )
    unsensed <- Editing.deleted(
      unexampled,
      List(Piece.Meaning(unexampled.senses(1), 1)),
      Timestamp(4),
    )
    unformed <- Editing.deleted(
      unsensed,
      List(Piece.Inflection(unsensed.forms.head, 0)),
      Timestamp(5),
    )
  yield unformed).get

  test(
    "A piece the reader reworded or deleted is kept out, and can be put back: the reworded text takes its place, and each piece taken out is rejected at its place, by its change.",
  ):
    assertEquals(
      edited.senses.map(_.id),
      List(anrufen.senses.head.id),
    )
    assertEquals(
      edited.senses.head.definitions.map(_.versions.map(_.text)),
      List(List("to telephone")),
    )
    assertEquals(
      edited.senses.head.definitions.head.versions.head.source,
      Source.Edited(Timestamp(2)),
    )
    assertEquals(
      edited.senses.head.examples,
      anrufen.senses.head.examples.tail,
    )
    assertEquals(edited.forms, anrufen.forms.tail)
    assertEquals(
      edited
        .rejected
        .map(rejected => rejected.at.millis -> rejected.piece.place),
      List(1L -> 0, 2L -> 0, 3L -> 0, 4L -> 1, 5L -> 0),
    )
    assertEquals(
      Editing.deleted(
        edited,
        List(firstDefinition(anrufen)),
        Timestamp(6),
      ),
      None,
    )

  test(
    "Fetched again, a lexeme keeps what the reader reworded in its place, and out what they deleted or replaced, each sense its own, though its only definition was replaced, and has the rest anew.",
  ):
    val merged = Derivation.merged(Some(edited), again("anrufen", 9).head)
    assertEquals(
      merged.senses.map(_.id),
      edited.senses.map(_.id),
    )
    assertEquals(
      merged.senses.head.definitions,
      edited.senses.head.definitions,
    )
    assertEquals(
      merged.senses.head.examples.map(_.text),
      edited.senses.head.examples.map(_.text),
    )
    assertEquals(
      merged.senses.head.examples.map(_.source),
      edited
        .senses
        .head
        .examples
        .map(_ => sourced("anrufen").copy(at = Timestamp(9))),
    )
    assertEquals(
      merged.forms.map(_.spelling),
      edited.forms.map(_.spelling),
    )
    assertEquals(merged.rejected, edited.rejected)
    val texts = (lexeme: Lexeme) =>
      lexeme
        .senses
        .map(sense => (sense.id, sense.definitions, sense.examples.map(_.text)))
    assertEquals(
      texts(Derivation.merged(Some(merged), again("anrufen", 10).head)),
      texts(merged),
    )

  test(
    "Undoing a change puts back what it took out, in its place, and takes out what it made, undoing first each later change to what it made.",
  ):
    assertEquals(
      List(5, 4, 3, 2, 1).foldLeft(edited)((lexeme, at) =>
        Editing.undone(lexeme, Timestamp(at)),
      ),
      anrufen,
    )
    assertEquals(
      Editing.undone(edited, Timestamp(1)).senses.head.definitions,
      anrufen.senses.head.definitions,
    )
    assertEquals(
      Editing.undone(edited, Timestamp(1)).rejected.map(_.at.millis),
      List(3L, 4L, 5L),
    )
    val unsensed = Editing.undone(edited, Timestamp(4))
    assertEquals(
      unsensed.senses.map(_.id),
      anrufen.senses.map(_.id),
    )
    assertEquals(
      unsensed.senses.head.definitions.head.versions.head.text,
      "to telephone",
    )
    assertEquals(
      Editing.undone(
        Editing.undone(edited, Timestamp(2)),
        Timestamp(1),
      ),
      Editing.undone(edited, Timestamp(1)),
    )

  test(
    "What a model wrote in place of a dictionary's piece is credited to the dictionary, hidden with it, and the card says the entry was changed.",
  ):
    val bank  = lexemes("Bank").last
    val sense = bank.senses.head
    val wrote = Source.Generated(
      Model.ClaudeHaiku4_5,
      None,
      Timestamp(3),
      List(sourced("Bank")),
    )
    val regenerated = Editing
      .replaced(
        bank,
        List(Piece.Meaning(sense, 0)),
        List(Piece.Meaning(
          sense.copy(
            id = SenseId(bank.id, "model", 0),
            definitions =
              List(Explanation.of(Language.English, "a bank", wrote)),
            source = wrote,
          ),
          0,
        )),
        Timestamp(3),
      )
      .get
    val off =
      (source: Source) => !source.dictionaries.contains(Dictionary.Wiktionary)
    val shown = Derivation.entry(
      regenerated,
      german,
      Language.English,
      all,
    )
    assertEquals(
      shown.senses.head.glosses,
      List("a bank"),
    )
    assertEquals(
      shown.source,
      models.Source.Wiktionary(sourced("Bank").url, false, true),
    )
    assertEquals(
      Derivation.entry(bank, german, Language.English, all).source,
      models.Source.Wiktionary(sourced("Bank").url, false, false),
    )
    assertEquals(
      Derivation
        .entry(
          regenerated,
          german,
          Language.English,
          off,
        )
        .senses,
      Nil,
    )
    assert(Derivation.sourced(regenerated, all))

  test(
    "Fetched again, and again, a lexeme of several senses keeps them in order, each its own, with what the reader reworded in place and what they deleted out.",
  ):
    val bench  = lexemes("Bank").head
    val second = bench.senses(1)
    val edited = (for
      reworded <- Editing.reworded(
        Piece.Definition(second.id, second.definitions.head, 0),
        "a bench for working at",
        None,
        Language.English,
        Timestamp(1),
      )
      once <- Editing.replaced(
        bench,
        List(Piece.Definition(second.id, second.definitions.head, 0)),
        List(reworded),
        Timestamp(1),
      )
      twice <- Editing.deleted(
        once,
        List(Piece.Meaning(once.senses(2), 2)),
        Timestamp(2),
      )
    yield twice).get
    val once  = Derivation.merged(Some(edited), again("Bank", 9).head)
    val twice = Derivation.merged(Some(once), again("Bank", 10).head)
    val shown = (lexeme: Lexeme) =>
      lexeme
        .senses
        .map(sense =>
          sense.id ->
            sense.definitions.flatMap(_.versions.headOption).map(_.text),
        )
    assert(
      edited.senses.size >= 3,
      edited.senses.size,
    )
    assertEquals(
      edited.senses.map(_.id),
      List(bench.senses(0), bench.senses(1)).map(_.id) ++
        bench.senses.drop(3).map(_.id),
    )
    assertEquals(shown(once), shown(edited))
    assertEquals(shown(twice), shown(edited))
    assertEquals(
      shown(twice)(1),
      second.id -> List("a bench for working at"),
    )
