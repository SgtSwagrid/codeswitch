package com.alecdorrington.codeswitch.lexicon

import cats.syntax.all.*
import com.alecdorrington.codeswitch.models
import com.alecdorrington.codeswitch.models.{whole, Entry, Forms, Wiktionary}
import com.alecdorrington.iris.Model
import com.alecdorrington.megalingual.Language
import io.circe.{ACursor, Json}
import io.circe.parser.parse

/**
  * How lexemes are made from what a dictionary or a model gives, merged with
  * those kept, and shown to a reader as a card's entry.
  */
object Derivation:

  /**
    * The lexemes in `lines`, the file of Kaikki's entries for a word of
    * `language` that `source` fetched: one for each entry with a word and a
    * word class, apart by the etymology that Wiktionary numbers it by, or else
    * by its place among those of its word class, but for an entry whose every
    * sense only points to another word (see [[pointers]]), as `ging`'s to
    * `gehen`, which is no word of its own.
    */
  def fromKaikki
    (
      language: Language,
      lines: String,
      source: Source.Sourced,
    )
    : List[Lexeme] =
    val entries = this.entries(lines)
    entries
      .zipWithIndex
      .flatMap((cursor, index) =>
        for
          word      <- cursor.get[String]("word").toOption
          wordClass <- cursor.get[String]("pos").toOption if glossed(cursor)
            .isEmpty || own(cursor).nonEmpty
        yield
          val before = entries
            .take(index)
            .count(other =>
              other.get[String]("word").toOption.contains(word) &&
              other.get[String]("pos").toOption.contains(wordClass),
            )
          lexeme(
            cursor,
            language,
            word,
            WordClass(wordClass),
            cursor.get[Int]("etymology_number").getOrElse(before + 1),
            source,
          ),
      )

  /** The entries in `lines`, a file of Kaikki's, each a line. */
  private def entries(lines: String): List[ACursor] = lines
    .linesIterator
    .filter(_.trim.nonEmpty)
    .flatMap(parse(_).toOption)
    .map(_.hcursor)
    .toList

  /** The senses of the entry at `cursor` that have glosses. */
  private def glossed(cursor: ACursor): List[ACursor] = Wiktionary
    .list(cursor.downField("senses"))(Some(_))
    .filter(sense => Wiktionary.strings(sense.downField("glosses")).nonEmpty)

  /**
    * The senses of the entry at `cursor` that are meanings of its own, not
    * pointers to another word (see [[pointing]]).
    */
  private def own(cursor: ACursor): List[ACursor] =
    glossed(cursor).filterNot(pointing)

  /**
    * Tags of a sense of another word's spelling that make it a meaning of its
    * own, as an abbreviation's, not merely another spelling of that word.
    */
  private val shortened = Set(
    "abbreviation",
    "acronym",
    "clipping",
    "contraction",
    "ellipsis",
    "initialism",
  )

  /**
    * Whether the sense at `cursor` only points to another word: is a form of it
    * (`form-of`, as `ging` is of `gehen`), or another spelling of it
    * (`alt-of`), not shortened (see [[shortened]]), as `talo` for `taloyhtiö`
    * is a meaning of its own.
    */
  private def pointing(sense: ACursor): Boolean =
    val tags = Wiktionary.strings(sense.downField("tags")).toSet
    tags("form-of") || tags("alt-of") && !tags.exists(shortened)

  /**
    * The words that the entries in `lines`, of `language`, are forms or other
    * spellings of (see [[pointing]]), as Wiktionary gives `ging` as of `gehen`:
    * each with the word class of the entry that points to it, each once.
    */
  def pointers(language: Language, lines: String): List[(WordClass, Spelling)] =
    entries(lines)
      .flatMap(cursor =>
        cursor
          .get[String]("pos")
          .toOption
          .toList
          .flatMap(wordClass =>
            glossed(cursor)
              .filter(pointing)
              .flatMap(sense =>
                List("form_of", "alt_of").flatMap(field =>
                  Wiktionary.list(
                    sense.downField(field),
                  )(_.get[String]("word").toOption.filter(_.trim.nonEmpty)),
                ),
              )
              .map(word => WordClass(wordClass) -> Spelling(language, word)),
          ),
      )
      .distinct

  private def lexeme
    (
      cursor: ACursor,
      language: Language,
      word: String,
      wordClass: WordClass,
      etymology: Int,
      source: Source.Sourced,
    )
    : Lexeme =
    val (spelling, used) = (Spelling(language, word), Set(language.whole))
    val id               = LexemeId(language, wordClass, etymology, spelling)
    val glossed          = own(cursor)
    Lexeme(
      id,
      Form(
        spelling,
        used,
        Set(Tag("canonical")),
        None,
        Wiktionary.list(
          cursor.downField("sounds"),
        )(pronunciation(used, source)),
        source,
      ),
      wordClass,
      Wiktionary
        .agreed(
          glossed.map(sense => Wiktionary.strings(sense.downField("tags"))),
        )
        .map(gender => Property(Tag(gender), source)),
      Wiktionary
        .forms(cursor)
        .map(form =>
          val spelling = Spelling(language, form.form)
          Form(
            spelling,
            used,
            form.tags.map(Tag(_)).toSet,
            form.source,
            Option
              .when(spelling.text != form.form)(
                Pronunciation(Some(form.form), None, None, used, source),
              )
              .toList,
            source,
          ),
        ),
      glossed
        .zipWithIndex
        .map((sense, index) =>
          this.sense(
            sense,
            language,
            SenseId(id, "wiktionary", index),
            source,
          ),
        ),
      cursor
        .get[String]("etymology_text")
        .toOption
        .filter(_.trim.nonEmpty)
        .map(Explanation.of(Language.English, _, source))
        .toList,
      relations(cursor, language, source)(
        "derived" -> Morphological.Derived,
        "related" -> Morphological.Related,
      ),
    )

  private def pronunciation
    (used: Set[Language], source: Source)
    (cursor: ACursor)
    : Option[Pronunciation] =
    val (ipa, recording) =
      (cursor.get[String]("ipa").toOption, Wiktionary.recording(cursor))
    Option.when(ipa.nonEmpty || recording.nonEmpty)(
      Pronunciation(None, ipa, recording, used, source),
    )

  private def sense
    (
      cursor: ACursor,
      language: Language,
      id: SenseId,
      source: Source,
    )
    : Sense = Sense(
    id,
    Set(language.whole),
    Wiktionary.strings(cursor.downField("tags")).map(Tag(_)).toSet,
    Wiktionary
      .strings(cursor.downField("glosses"))
      .map(Explanation.of(Language.English, _, source)),
    Wiktionary.list(cursor.downField("examples"))(example(source)),
    relations(cursor, language, source)(
      "synonyms"  -> Semantic.Synonym,
      "antonyms"  -> Semantic.Antonym,
      "hypernyms" -> Semantic.Broader,
      "hyponyms"  -> Semantic.Narrower,
    ),
    source,
  )

  private def example(source: Source)(cursor: ACursor): Option[Example] = cursor
    .get[String]("text")
    .toOption
    .filter(_.trim.nonEmpty)
    .map(text =>
      Example(
        text,
        cursor
          .get[String]("translation")
          .orElse(cursor.get[String]("english"))
          .toOption
          .filter(_.trim.nonEmpty)
          .fold(Explanation.none)(Explanation.of(Language.English, _, source)),
        cursor.get[String]("ref").toOption.filter(_.trim.nonEmpty),
        source,
      ),
    )

  /** The words that the lists at `cursor` named by `kinds` relate to it. */
  private def relations[K]
    (
      cursor: ACursor,
      language: Language,
      source: Source,
    )
    (kinds: (String, K)*)
    : List[Relation[K]] = kinds
    .toList
    .flatMap((field, kind) =>
      Wiktionary
        .list(cursor.downField(field))(_.get[String]("word").toOption)
        .filter(_.trim.nonEmpty)
        .map(word => Relation(kind, Spelling(language, word), source)),
    )

  /**
    * The lexeme of `entry`, of a word of `learnt`, that `model` generated in
    * `native`, the reader's own language, at `at`: of etymology `0`, which no
    * dictionary numbers one by, so that it is never taken for a dictionary's
    * lexeme of the word, as the bench for the bank.
    */
  def generated
    (
      entry: Entry,
      learnt: Language,
      native: Language,
      model: Model,
      at: Timestamp,
    )
    : Lexeme =
    val source    = Source.Generated(model, None, at)
    val spelling  = Spelling(learnt, entry.word)
    val wordClass = WordClass(entry.wordClass)
    val used      = Set(learnt.whole)
    val id        = LexemeId(learnt, wordClass, 0, spelling)
    Lexeme(
      id,
      Form(
        spelling,
        used,
        Set(Tag("canonical")),
        None,
        entry
          .ipa
          .map(ipa => Pronunciation(None, Some(ipa), None, used, source))
          .toList,
        source,
      ),
      wordClass,
      entry.genders.map(gender => Property(Tag(gender), source)),
      entry
        .forms
        .map(form =>
          Form(
            Spelling(learnt, form.form),
            used,
            form.tags.map(Tag(_)).toSet,
            form.source,
            Nil,
            source,
          ),
        ),
      entry
        .senses
        .zipWithIndex
        .map((sense, index) =>
          Sense(
            SenseId(
              id,
              s"model-${ native.code }-${ at.millis }",
              index,
            ),
            used,
            Set.empty,
            sense.glosses.map(Explanation.of(native, _, source)),
            sense
              .examples
              .map(example =>
                Example(
                  example.text,
                  example
                    .translation
                    .fold(Explanation.none)(Explanation.of(native, _, source)),
                  None,
                  source,
                ),
              ),
            Nil,
            source,
          ),
        ),
      entry.etymology.map(Explanation.of(native, _, source)).toList,
      Nil,
    )

  /**
    * `found`, a lexeme as a source now gives it, merged into `kept`, the same
    * lexeme as kept before, if any: what that source gave before is replaced,
    * but for each translation of a text it still gives alike, and the rest is
    * kept, as what other sources and models gave.
    */
  def merged(kept: Option[Lexeme], found: Lexeme): Lexeme =
    kept.fold(found)(kept =>
      val urls = found.records
      val from = (claim: Attributed) => recordOf(claim.source).exists(urls)
      val translations =
        (kept.senses.flatMap(_.claims) ++ kept.etymologies.flatMap(_.versions))
          .collect {
            case version @ Version(_, _, Source.Translated(_, original, _, _))
              if from(version) => original -> version
          }
          .groupMap(_._1)(_._2)
      val again = (explanation: Explanation) =>
        explanation
          .original
          .fold(explanation)(original =>
            explanation.copy(versions =
              explanation.versions ++
                translations
                  .getOrElse(original.text, Nil)
                  .filterNot(version =>
                    explanation.versions.exists(_.language == version.language),
                  ),
            ),
          )
      Lexeme(
        kept.id,
        if from(kept.lemma) || !isSourced(kept.lemma.source) then found.lemma
        else kept.lemma,
        kept.wordClass,
        kept.properties.filterNot(from) ++ found.properties,
        kept.forms.filterNot(from) ++ found.forms,
        kept.senses.filterNot(from) ++
          found
            .senses
            .map(sense =>
              sense.copy(
                definitions = sense.definitions.map(again),
                examples = sense
                  .examples
                  .map(example =>
                    example.copy(translation = again(example.translation)),
                  ),
              ),
            ),
        kept.etymologies.filterNot(_.versions.exists(from)) ++
          found.etymologies.map(again),
        kept.relations.filterNot(from) ++ found.relations,
      ),
    )

  /** The URL of the record that `source` is, or was translated from, if any. */
  def recordOf(source: Source): Option[String] = source match
    case Source.Sourced(_, url, _)                             => Some(url)
    case Source.Translated(Source.Sourced(_, url, _), _, _, _) => Some(url)
    case Source.Generated(_, _, _)                             => None

  /**
    * Whether `lexeme` has a sense from a dictionary that `shown` lets a card
    * show.
    */
  def sourced(lexeme: Lexeme, shown: Source => Boolean): Boolean = lexeme
    .senses
    .exists(sense => shown(sense.source) && isSourced(sense.source))

  private def isSourced(source: Source): Boolean = source match
    case Source.Sourced(_, _, _) => true
    case _                       => false

  /**
    * The dictionary that `source` is from, or was translated from, if any: a
    * model's generating is of none.
    */
  def dictionaryOf(source: Source): Option[Dictionary] = source match
    case Source.Sourced(dictionary, _, _) => Some(dictionary)
    case Source.Translated(Source.Sourced(dictionary, _, _), _, _, _) =>
      Some(dictionary)
    case Source.Generated(_, _, _) => None

  /**
    * The entry that a card shows of `lexeme`, a word of `learnt`, to a reader
    * of `native`, of only what `shown` lets it show, as what the dictionaries
    * they turned off gave is not: its senses that can be read in their language
    * or else in English, each explanation in theirs if it has a version in
    * theirs, those from dictionaries before those that a model generated, and
    * the forms of one source alone, a dictionary's if it gave any; and that it
    * was translated only if a text it shows was.
    */
  def entry
    (
      lexeme: Lexeme,
      learnt: Language,
      native: Language,
      shown: Source => Boolean,
    )
    : Entry =
    val read = (explanation: Explanation) =>
      Some(explanation.versions.filter(version => shown(version.source)))
        .map(Explanation(_))
        .flatMap(shown => shown.in(native).orElse(shown.in(Language.English)))
    val readable = lexeme
      .senses
      .filter(sense => shown(sense.source))
      .sortBy(sense => !isSourced(sense.source))
      .map(sense =>
        (
          sense.definitions.flatMap(read),
          sense
            .examples
            .filter(example => shown(example.source))
            .map(example => example -> read(example.translation)),
        ),
      )
      .filter(_._1.nonEmpty)
    val etymology = lexeme.etymologies.flatMap(read).headOption
    val worded    = readable.flatMap((definitions, examples) =>
      definitions ++ examples.flatMap(_._2),
    ) ++ etymology
    val senses = readable.map((definitions, examples) =>
      models.Sense(
        definitions.map(_.text),
        examples.map((example, translation) =>
          models.Example(example.text, translation.map(_.text)),
        ),
      ),
    )
    val said = lexeme
      .lemma
      .pronunciations
      .filter(pronunciation => shown(pronunciation.source))
    val forms                = lexeme.forms.filter(form => shown(form.source))
    val (sourced, generated) = forms.partition(form => isSourced(form.source))
    val properties           = lexeme
      .properties
      .filter(property => shown(property.source))
    Entry(
      lexeme.lemma.spelling.text,
      lexeme.wordClass.name,
      Entry.gendersOf(
        Some(properties.filter(property => isSourced(property.source)))
          .filter(_.nonEmpty)
          .getOrElse(properties)
          .map(_.tag.name),
      ),
      senses,
      Forms.kept(
        learnt,
        lexeme.wordClass.name,
        (if sourced.nonEmpty then sourced else generated).map(form =>
          models.Form(
            form
              .pronunciations
              .flatMap(_.marked)
              .headOption
              .getOrElse(form.spelling.text),
            form.tags.toList.map(_.name),
            form.table,
          ),
        ),
      ),
      said.flatMap(_.ipa).headOption,
      Wiktionary.audible(
        learnt,
        said.flatMap(_.recording).distinct,
      ),
      etymology.map(_.text),
      lexeme
        .senses
        .filter(sense => shown(sense.source))
        .map(_.source)
        .collectFirst { case Source.Sourced(_, url, _) =>
          models
            .Source
            .Wiktionary(
              url,
              translated = worded.exists(
                _.source match
                  case Source.Translated(_, _, _, _) => true
                  case _                             => false,
              ),
            )
        }
        .getOrElse(models.Source.Model),
    )

  /**
    * The entry of `lexeme`, a word of `learnt`, in English, as it is put into
    * another language (see [[translated]]): each of the senses that `shown`
    * lets a card show, if it is defined in English from a source, with each of
    * its examples, and its first etymology so defined, if any.
    */
  def english
    (
      lexeme: Lexeme,
      learnt: Language,
      shown: Source => Boolean,
    )
    : Entry = entry(
    lexeme.copy(
      senses = translatable(lexeme, shown),
      etymologies = lexeme.etymologies.filter(english(_).nonEmpty).take(1),
    ),
    learnt,
    Language.English,
    source => shown(source) && isSourced(source),
  )

  /** The senses of `lexeme` that [[english]] gives, in order. */
  private def translatable
    (lexeme: Lexeme, shown: Source => Boolean)
    : List[Sense] = lexeme
    .senses
    .filter(sense =>
      shown(sense.source) && isSourced(sense.source) &&
      sense.definitions.exists(english(_).nonEmpty),
    )

  /** The version of `explanation` in English that a source gave, if any. */
  private def english(explanation: Explanation): Option[Version] = explanation
    .original
    .filter(_.language == Language.English)

  /**
    * `lexeme`, with `translation`, the entry that [[english]] gave of it put
    * into `native` by `model` at `at`, in versions of its texts: each text that
    * a source gave in English, and that has no version in `native` yet, given
    * the text in that place in `translation`, if it has one there.
    */
  def translated
    (
      lexeme: Lexeme,
      translation: Entry,
      native: Language,
      model: Model,
      at: Timestamp,
      shown: Source => Boolean,
    )
    : Lexeme =
    val senses = translatable(lexeme, shown)
    val put    = (
      explanation: Explanation,
      text: Option[String],
    ) =>
      english(explanation)
        .filter(_ => explanation.in(native).isEmpty)
        .flatMap(original =>
          (original.source, text.filter(_.trim.nonEmpty)) match
            case (from @ Source.Sourced(_, _, _), Some(text)) => Some(Version(
                native,
                text.trim,
                Source.Translated(from, original.text, model, at),
              ))
            case _ => None,
        )
        .fold(explanation)(version =>
          explanation.copy(versions = explanation.versions :+ version),
        )
    val translations = senses.map(_.id).zip(translation.senses).toMap
    lexeme.copy(
      senses = lexeme
        .senses
        .map(sense =>
          translations
            .get(sense.id)
            .fold(sense)(translated =>
              val glosses = translated.glosses.map(Some(_)) ++
                List.fill(sense.definitions.size)(None)
              val examples = translated.examples.map(Some(_)) ++
                List.fill(sense.examples.size)(None)
              sense.copy(
                definitions = sense
                  .definitions
                  .zip(glosses)
                  .map((definition, gloss) => put(definition, gloss)),
                examples = sense
                  .examples
                  .zip(examples)
                  .map((example, translated) =>
                    example.copy(translation =
                      put(
                        example.translation,
                        translated.flatMap(_.translation),
                      ),
                    ),
                  ),
              ),
            ),
        ),
      etymologies = lexeme
        .etymologies
        .zipWithIndex
        .map((etymology, index) =>
          if index == lexeme.etymologies.indexWhere(english(_).nonEmpty) then
            put(etymology, translation.etymology)
          else etymology,
        ),
    )

  /**
    * Whether `lexeme` has a text that a card would show a reader of `native` in
    * English, as it has no version in theirs, that a source gave in English and
    * a model could put into theirs.
    */
  def untranslated
    (
      lexeme: Lexeme,
      native: Language,
      shown: Source => Boolean,
    )
    : Boolean =
    val missing = (explanation: Explanation) =>
      english(explanation).nonEmpty && explanation.in(native).isEmpty
    translatable(lexeme, shown).exists(sense =>
      sense.definitions.exists(missing) ||
      sense.examples.exists(example => missing(example.translation)),
    ) || lexeme.etymologies.find(english(_).nonEmpty).exists(missing)
