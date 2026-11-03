package com.alecdorrington.codeswitch.lexicon

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
  )
  derives Codec:

  /** The languages that use it, as its forms are spelt in. */
  def languages: Set[Language] = (lemma :: forms).flatMap(_.languages).toSet

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
      case Source.Translated(Source.Sourced(_, url, _), _, _, _) => url
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
  extends Attributed derives Codec

/**
  * How a form is said in `languages`: its spelling with the marks that show it
  * (`до́ма`), its `ipa`, and a recording of it at `audio`.
  */
final case class Pronunciation
  (
    marked: Option[String],
    ipa: Option[String],
    audio: Option[String],
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
