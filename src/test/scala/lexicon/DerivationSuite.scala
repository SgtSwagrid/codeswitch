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
        ),
      )),
      Nil,
      Nil,
      Source.Generated(
        Model.ClaudeHaiku4_5,
        None,
        Timestamp(2),
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
    val off  = (source: Source) =>
      !Derivation.dictionaryOf(source).contains(Dictionary.Wiktionary)
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
