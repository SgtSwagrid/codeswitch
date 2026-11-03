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
      Nil,
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
    val source    = Source.Generated(model, Nil, None, at)
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
      Nil,
    )

  /**
    * `found`, a lexeme as a source now gives it, merged into `kept`, the same
    * lexeme as kept before, if any, in its order: without what the reader or a
    * review took out of it (see [[Editing]]), each of its claims that the
    * source gave before replaced by the claim that continues it, keeping what
    * other sources, models and the reader put within it, as each translation of
    * a text still given alike, or left out if none does; what the source gives
    * that continues none after; and the rest kept as it was.
    */
  def merged(kept: Option[Lexeme], found: Lexeme): Lexeme =
    kept.fold(found)(kept =>
      val urls     = found.records
      val from     = (claim: Attributed) => recordOf(claim.source).exists(urls)
      val rejected = kept.rejected
      kept.copy(
        lemma =
          if from(kept.lemma) || !isSourced(kept.lemma.source) then found.lemma
          else kept.lemma,
        properties = kept.properties.filterNot(from) ++ found.properties,
        forms = continued(
          kept.forms,
          found.forms.filterNot(rejects(rejected)),
        )(from, sameForm, never, (_, again) => again),
        senses = renewed(
          continued(
            kept.senses,
            found.senses.filterNot(rejects(rejected)),
          )(
            from,
            (old, again) =>
              recorded(old, rejected).exists(again.originals.toSet),
            (old, again) => old.id == again.id,
            carried(from, rejected),
          ).map(pruned(rejected)).filter(_.definitions.nonEmpty),
        ),
        etymologies = continued(
          kept.etymologies,
          found.etymologies.filterNot(rejects(rejected)),
        )(
          _.versions.exists(from),
          sameOriginal,
          never,
          translations,
        ),
        relations = kept.relations.filterNot(from) ++ found.relations,
      ),
    )

  /**
    * `kept`, in order, with each from the record (`fromRecord`) replaced by
    * `carry` of it and the one of `fresh` that continues it, by `continues`, or
    * else by `alike`, each of `fresh` continuing one at most, or left out if
    * none does; then the rest of `fresh`.
    */
  private def continued[A]
    (kept: List[A], fresh: List[A])
    (
      fromRecord: A => Boolean,
      continues: (A, A) => Boolean,
      alike: (A, A) => Boolean,
      carry: (A, A) => A,
    )
    : List[A] =
    val pairs = List(continues, alike).foldLeft(Map.empty[Int, Int])(matched(
      kept.map(Some(_).filter(fromRecord)),
      fresh,
    ))
    kept
      .zipWithIndex
      .flatMap((old, index) =>
        if fromRecord(old) then
          pairs.get(index).map(at => carry(old, fresh(at)))
        else Some(old),
      ) ++ fresh.indices.filterNot(pairs.values.toSet).map(fresh)

  /**
    * `pairs`, with each of `kept` not yet paired paired with the first of
    * `fresh` not yet paired that `test` finds continues it, by their places.
    */
  private def matched[A]
    (kept: List[Option[A]], fresh: List[A])
    (
      pairs: Map[Int, Int],
      test: (A, A) => Boolean,
    )
    : Map[Int, Int] = kept
    .zipWithIndex
    .foldLeft(pairs)((pairs, entry) =>
      entry match
        case (Some(old), index) if !pairs.contains(index) =>
          fresh
            .indices
            .find(at => !pairs.values.exists(_ == at) && test(old, fresh(at)))
            .fold(pairs)(pairs.updated(index, _))
        case _ => pairs,
    )

  /** That nothing continues anything. */
  private def never[A](old: A, again: A): Boolean = false

  /** Whether two forms are of the same spelling, tags and table. */
  private def sameForm(old: Form, again: Form): Boolean =
    (old.spelling, old.tags, old.table) ==
      (again.spelling, again.tags, again.table)

  /** Whether two explanations are of the same text as their sources gave it. */
  private def sameOriginal(old: Explanation, again: Explanation): Boolean =
    old.original.nonEmpty &&
    old.original.map(_.text) == again.original.map(_.text)

  /**
    * The texts of the definitions that `sense` was given by its source, with
    * those among them that the reader replaced, as `rejected` keeps them, by
    * which the sense is known when given again.
    */
  private def recorded(sense: Sense, rejected: List[Rejected]): List[String] =
    sense.originals ++ rejected.flatMap {
      case Rejected(Piece.Definition(id, definition, _), _, _)
        if id == sense.id => definition.original.map(_.text)
      case _ => None
    }

  /**
    * `again`, a sense as its source now gives it, without what `rejected`
    * holds, with the own of `old`, the sense that it continues, and what the
    * reader, other sources and models put within that.
    */
  private def carried
    (
      from: Attributed => Boolean,
      rejected: List[Rejected],
    )
    (old: Sense, again: Sense)
    : Sense =
    val fresh = pruned(rejected)(again)
    fresh.copy(
      id = old.id,
      definitions = continued(old.definitions, fresh.definitions)(
        _.versions.exists(from),
        sameOriginal,
        never,
        translations,
      ),
      examples = continued(old.examples, fresh.examples)(
        from,
        (old, example) => old.text == example.text,
        never,
        (old, example) =>
          example.copy(translation =
            translations(old.translation, example.translation),
          ),
      ),
      relations = fresh.relations ++ old.relations.filterNot(from),
    )

  /**
    * `fresh`, an explanation as its source now gives it, with the versions of
    * `old`, which it continues, in other languages, as translations of a text
    * still given alike.
    */
  private def translations(old: Explanation, fresh: Explanation): Explanation =
    fresh.copy(versions =
      fresh.versions ++
        old
          .versions
          .filterNot(version =>
            fresh.versions.exists(_.language == version.language),
          ),
    )

  /** `sense`, without the definitions and examples that `rejected` holds. */
  private def pruned(rejected: List[Rejected])(sense: Sense): Sense = sense
    .copy(
      definitions = sense.definitions.filterNot(rejects(rejected)),
      examples = sense.examples.filterNot(rejects(rejected)),
    )

  /** `senses`, each whose own a sense before it has given another. */
  private def renewed(senses: List[Sense]): List[Sense] = senses
    .foldLeft((List.empty[Sense], Set.empty[SenseId]))((done, sense) =>
      val (senses, taken) = done
      val id              = SenseId.free(sense.id, taken)
      (sense.copy(id = id) :: senses, taken + id),
    )
    ._1
    .reverse

  /**
    * Whether `rejected` holds what a source gives again as `item`: a piece of
    * its kind from the same record, of the same text, or spelling, or a sense
    * from the same record with a definition of the same text.
    */
  private def rejects
    (rejected: List[Rejected])
    (item: Attributed | Explanation)
    : Boolean = rejected
    .map(_.piece)
    .exists(piece =>
      (piece, item) match
        case (Piece.Meaning(old, _), sense: Sense) => sameRecord(old, sense) &&
          recorded(old, rejected).exists(sense.originals.toSet)
        case (Piece.Definition(_, old, _), explanation: Explanation) =>
          sameText(old, explanation)
        case (Piece.Origin(old, _), explanation: Explanation) =>
          sameText(old, explanation)
        case (Piece.Usage(_, old, _), example: Example) =>
          sameRecord(old, example) && old.text == example.text
        case (Piece.Inflection(old, _), form: Form) => sameRecord(old, form) &&
          sameForm(old, form)
        case _ => false,
    )

  /** Whether two claims are from one record, however long ago it was fetched. */
  private def sameRecord(old: Attributed, again: Attributed): Boolean =
    recordOf(old.source).exists(recordOf(again.source).contains)

  /** Whether two explanations are of one text, as one record gave it. */
  private def sameText(old: Explanation, again: Explanation): Boolean = old
    .original
    .zip(again.original)
    .exists((old, again) => sameRecord(old, again) && old.text == again.text)

  /**
    * The URL of the record that `source` is, or was translated from, if any:
    * none for what a model generated or the reader wrote, which no record gives
    * again.
    */
  def recordOf(source: Source): Option[String] = source match
    case Source.Sourced(_, url, _)                             => Some(url)
    case Source.Translated(_, Source.Sourced(_, url, _), _, _) => Some(url)
    case _                                                     => None

  /**
    * Whether `lexeme` has a sense that `shown` lets a card show credited to a
    * dictionary, as one from it, or a model's rewriting of one of its.
    */
  def sourced(lexeme: Lexeme, shown: Source => Boolean): Boolean = lexeme
    .senses
    .exists(sense => shown(sense.source) && sense.source.credits.nonEmpty)

  private def isSourced(source: Source): Boolean = source match
    case Source.Sourced(_, _, _) => true
    case _                       => false

  /**
    * The entry that a card shows of `lexeme`, a word of `learnt`, to a reader
    * of `native`, of only what `shown` lets it show, as what the dictionaries
    * they turned off gave is not: its senses that can be read in their language
    * or else in English, in order, each explanation in theirs if it has a
    * version in theirs, and its forms; that it was translated only if a text it
    * shows was, and changed if the reader or a model rewrote one, or took one
    * out; and the dictionary it is credited to, if any.
    */
  def entry
    (
      lexeme: Lexeme,
      learnt: Language,
      native: Language,
      shown: Source => Boolean,
    )
    : Entry =
    val read        = reading(native, shown)
    val shownSenses = lexeme.senses.filter(sense => shown(sense.source))
    val readable    = shownSenses
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
    val changed = lexeme.rejected.nonEmpty ||
      (worded ++ shownSenses ++ readable.flatMap(_._2.map(_._1))).exists(
        _.source.change.nonEmpty,
      )
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
    val properties = lexeme
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
        lexeme
          .forms
          .filter(form => shown(form.source))
          .map(form =>
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
      shownSenses
        .flatMap(_.source.credits)
        .headOption
        .fold(models.Source.Model)(credit =>
          models
            .Source
            .Wiktionary(
              credit.url,
              translated = worded.exists(
                _.source match
                  case Source.Translated(_, _, _, _) => true
                  case _                             => false,
              ),
              changed,
            ),
        ),
    )

  /**
    * What reads `explanation` for a reader of `native`, of only what `shown`
    * lets them see: its version in their language, or else in English.
    */
  def reading
    (native: Language, shown: Source => Boolean)
    (explanation: Explanation)
    : Option[Version] =
    Explanation(explanation.versions.filter(version => shown(version.source)))
      .read(native)

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
                Source.Translated(model, from, original.text, at),
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
