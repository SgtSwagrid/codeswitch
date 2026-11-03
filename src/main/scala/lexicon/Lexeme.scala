package com.alecdorrington.codeswitch.lexicon

import com.alecdorrington.codeswitch.models.{Entry, Recording}
import com.alecdorrington.megalingual.Language
import io.circe.Codec

/**
  * A word, in all its forms and senses, as every source that knows it gives it,
  * in whichever languages use it. Words spelt alike are lexemes apart, as
  * `Bank`, a bench, and `Bank`, a bank, each with an `id` of its own.
  *
  * @param lemma
  *   Its dictionary form.
  *
  * @param properties
  *   What is true of it in every form and sense, as its gender.
  *
  * @param relations
  *   The words related to it in form, as those derived from it.
  *
  * @param rejected
  *   The pieces of it that the reader or a review took out, which a source
  *   giving them again does not bring back, and which may be put back.
  */
final case class Lexeme
  (
    id: LexemeId,
    lemma: Form,
    wordClass: WordClass,
    properties: List[Property],
    forms: List[Form],
    senses: List[Sense],
    etymologies: List[Explanation],
    relations: List[Relation[Morphological]],
    rejected: List[Rejected],
  )
  derives Codec:

  /** The languages that use it, as its forms are spelt in. */
  def languages: Set[Language] = (lemma :: forms).flatMap(_.languages).toSet

  /**
    * Its forms that are spellings of it, its dictionary form first, and not
    * those that are other words, which a dictionary names beside them (see
    * [[Form.besides]]).
    */
  def spellings: List[Form] = lemma :: forms.filterNot(_.besides(wordClass))

  /** Every piece of information about it. */
  def claims: List[Attributed] = lemma ::
    lemma.pronunciations ++
    properties ++ forms ++ forms.flatMap(_.pronunciations) ++
    senses.flatMap(_.claims) ++ etymologies.flatMap(_.versions) ++ relations

  /** The URLs of the records that it was derived from. */
  def records: Set[String] = claims
    .map(_.source)
    .collect {
      case Source.Sourced(_, url, _)                             => url
      case Source.Translated(_, Source.Sourced(_, url, _), _, _) => url
    }
    .toSet

  /**
    * It, with each of its explanations that has no version in `language` given
    * one by `translate`, if it gives any.
    */
  def translated(translate: Explanation => Option[Version]): Lexeme = copy(
    senses = senses.map(_.translated(translate)),
    etymologies = etymologies.map(_.translated(translate)),
  )

/**
  * A grammatical property of a lexeme in every form and sense: its gender,
  * aspect, class and the like (`feminine`, `perfective`, `uncountable`).
  */
final case class Property(tag: Tag, source: Source) extends Attributed
  derives Codec

/**
  * A form of a lexeme: how it is spelt in `languages` (`Strasse` in
  * Switzerland, `Straße` in German at large), and what it is (`genitive`,
  * `plural`).
  *
  * @param table
  *   The table of its lexeme's forms it is laid out in, as Wiktionary names its
  *   template (`de-decl-noun-f`), or `model` for one a model wrote, if any; a
  *   form of the dictionary's heading has none.
  */
final case class Form
  (
    spelling: Spelling,
    languages: Set[Language],
    tags: Set[Tag],
    table: Option[String],
    pronunciations: List[Pronunciation],
    source: Source,
  )
  extends Attributed derives Codec:

  /**
    * Whether it is another word, a lexeme of its own, that a dictionary names
    * beside the forms of one of `wordClass`, rather than a spelling of it: a
    * verb's auxiliary (`haben`), a word made from it (`Bänkchen`, `говорение`,
    * `домашний`), a verb of its other aspect (`сказать` of `говорить`), or a
    * noun of another gender (`kocica` of `kot`).
    */
  def besides(wordClass: WordClass): Boolean =
    val names = tags.map(_.name)
    names.exists(Form.others) ||
    table.isEmpty && names.exists(Form.aspects) && !names("canonical") ||
    wordClass.name == "noun" &&
    table.isEmpty && names.nonEmpty && names.subsetOf(Entry.genders.toSet)

object Form:

  /** Tags of a form that is another word, wherever it is given. */
  private val others = Set(
    "auxiliary",
    "diminutive",
    "augmentative",
    "pejorative",
    "endearing",
    "noun-from-verb",
    "adjective",
    "relational",
  )

  /** Tags of a heading's form that is a verb of another aspect. */
  private val aspects = Set("perfective", "imperfective")

/**
  * How a form is said in `languages`: its spelling with the marks that show it
  * (`до́ма`), its `ipa`, and a speaker's `recording` of it.
  */
final case class Pronunciation
  (
    marked: Option[String],
    ipa: Option[String],
    recording: Option[Recording],
    languages: Set[Language],
    source: Source,
  )
  extends Attributed derives Codec

/**
  * A meaning of a lexeme, as one source divides them, in `languages`: its
  * `definitions`, its `examples`, the words related to it in meaning, and the
  * field, register and grammar it is used in (`computing`, `colloquial`,
  * `in-plural`, `neuter`).
  */
final case class Sense
  (
    id: SenseId,
    languages: Set[Language],
    labels: Set[Tag],
    definitions: List[Explanation],
    examples: List[Example],
    relations: List[Relation[Semantic]],
    source: Source,
  )
  extends Attributed derives Codec:

  /** It, and every piece of information within it. */
  def claims: List[Attributed] = this ::
    definitions.flatMap(_.versions) ++
    examples.flatMap(example => example :: example.translation.versions) ++
    relations

  /** The texts of its definitions as their sources gave them. */
  def originals: List[String] = definitions.flatMap(_.original).map(_.text)

  /** It, each of its explanations given a version by `translate` if missing. */
  def translated(translate: Explanation => Option[Version]): Sense = copy(
    definitions = definitions.map(_.translated(translate)),
    examples = examples.map(example =>
      example.copy(translation = example.translation.translated(translate)),
    ),
  )

/**
  * A sense in use: `text`, a sentence in the lexeme's language, what it means,
  * and the work it is quoted from, if its source cites one.
  */
final case class Example
  (
    text: String,
    translation: Explanation,
    reference: Option[String],
    source: Source,
  )
  extends Attributed derives Codec

/**
  * One explanation of something, in each language it is worded in: as a source
  * or a model gave it, and translations of that.
  */
final case class Explanation(versions: List[Version]) derives Codec:

  /**
    * Its version in `language`: one in it, else in a broader language that it
    * belongs to, the nearest first, but never in a sibling variety.
    */
  def in(language: Language): Option[Version] = language
    .lineage
    .flatMap(each => versions.find(_.language == each))
    .headOption

  /**
    * Its version that a reader of `native` reads: in their language, or else in
    * English.
    */
  def read(native: Language): Option[Version] =
    in(native).orElse(in(Language.English))

  /** Its version from a source, from which others are translated. */
  def original: Option[Version] = versions.find(
    _.source match
      case Source.Sourced(_, _, _) => true
      case _                       => false,
  )

  /** It, with a version given by `translate`, if it gives one. */
  def translated(translate: Explanation => Option[Version]): Explanation =
    translate(this).fold(this)(version => copy(versions = versions :+ version))

object Explanation:

  /** An explanation worded only in `language`, as `source` gave it. */
  def of
    (
      language: Language,
      text: String,
      source: Source,
    )
    : Explanation = Explanation(List(Version(language, text, source)))

  /** None, as of an example that is not translated. */
  val none: Explanation = Explanation(Nil)

/** An [[Explanation]] as worded in `language`. */
final case class Version
  (
    language: Language,
    text: String,
    source: Source,
  )
  extends Attributed derives Codec

/**
  * A word related to a lexeme, or to a sense of it, spelt `spelling`, as `kind`
  * relates them.
  */
final case class Relation[K]
  (
    kind: K,
    spelling: Spelling,
    source: Source,
  )
  extends Attributed derives Codec

/** How a word relates to a sense: in meaning. */
enum Semantic derives Codec:
  case Synonym, Antonym, Broader, Narrower

/** How a word relates to a lexeme: in form. */
enum Morphological derives Codec:
  case Derived, Related

/**
  * A piece of a lexeme that the reader may take out, reword, or have a model
  * write again: what it holds, and its `place` among those like it.
  */
enum Piece derives Codec:

  /** A sense, with all that it holds. */
  case Meaning(sense: Sense, place: Int)

  /** A definition of the sense `sense`. */
  case Definition
    (
      sense: SenseId,
      definition: Explanation,
      place: Int,
    )

  /** An example of the sense `sense`. */
  case Usage
    (
      sense: SenseId,
      example: Example,
      place: Int,
    )

  /** An account of where the lexeme came from. */
  case Origin(etymology: Explanation, place: Int)

  /** A form of the lexeme. */
  case Inflection(form: Form, place: Int)

  def place: Int

  /** Every piece of information that it holds. */
  def claims: List[Attributed] = this match
    case Meaning(sense, _)            => sense.claims
    case Definition(_, definition, _) => definition.versions
    case Usage(_, example, _)         => example :: example.translation.versions
    case Origin(etymology, _)         => etymology.versions
    case Inflection(form, _)          => form :: form.pronunciations

  /** It, at `place` instead. */
  def at(place: Int): Piece = this match
    case piece: Meaning    => piece.copy(place = place)
    case piece: Definition => piece.copy(place = place)
    case piece: Usage      => piece.copy(place = place)
    case piece: Origin     => piece.copy(place = place)
    case piece: Inflection => piece.copy(place = place)

/**
  * A piece taken out of a lexeme by the change made at `at`, by the reader or
  * by a review, in place of which that change may have put something of its own
  * (see [[Source.change]]). A source that gives it again does not bring it back
  * (see [[Derivation.merged]]), and undoing the change puts it back.
  */
final case class Rejected
  (
    piece: Piece,
    at: Timestamp,
    reason: Reason,
  )
  derives Codec

/** Why a lexeme was changed, and so why a piece was taken out of it. */
enum Reason derives Codec:

  /** The reader took it out. */
  case Deleted

  /** The reader reworded it. */
  case Reworded

  /** A model wrote it again, as the reader asked. */
  case Regenerated

  /**
    * A model's review merged it with what repeats it, or found it no longer
    * needed.
    */
  case Reviewed

  /**
    * A model added a sense, as the reader asked, taking nothing out, so that no
    * rejected piece is of it.
    */
  case Added
