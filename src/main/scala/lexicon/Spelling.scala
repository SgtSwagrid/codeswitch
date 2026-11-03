package com.alecdorrington.codeswitch.lexicon

import com.alecdorrington.codeswitch.models.whole
import com.alecdorrington.megalingual.Language
import io.circe.{Codec, Decoder, Encoder}
import scala.scalajs.js.JSStringOps.enableJSStringOps
import scala.scalajs.js.UnicodeNormalizationForm

/**
  * How a word is written in its language's ordinary orthography, without the
  * marks that only show how it is said: `дома`, never `до́ма`.
  */
opaque type Spelling = String

object Spelling:

  /**
    * `text` as `language` spells it: trimmed and composed (NFC), and without
    * the marks of stress that dictionaries add to the words of a language whose
    * ordinary orthography has none (see [[stresses]]).
    */
  def apply(language: Language, text: String): Spelling =
    val trimmed = text.trim
    stresses(language).fold(trimmed.normalize(UnicodeNormalizationForm.NFC))(
      marks =>
        trimmed
          .normalize(UnicodeNormalizationForm.NFD)
          .replaceAll(marks, "")
          .normalize(UnicodeNormalizationForm.NFC),
    )

  /**
    * The combining marks, as a class of a regular expression, with which
    * dictionaries mark the stress of `language`'s words and that it never
    * writes, if any: Russian's and Ukrainian's acute and grave, and Bulgarian's
    * acute alone, as its grave is spelt in `ѝ`, her, apart from `и`, and.
    */
  private def stresses(language: Language): Option[String] =
    language.whole match
      case Language.Russian | Language.Ukrainian => Some("[̀́]")
      case Language.Bulgarian                    => Some("́")
      case _                                     => None

  extension (spelling: Spelling)

    /** The spelling as text. */
    def text: String = spelling

  given Codec[Spelling] = Codec.from(
    Decoder.decodeString,
    Encoder.encodeString,
  )

/**
  * Every lexeme spelt `spelling`, of any language, and when each language's
  * sources were last asked about it (`checked`), as one found only in other
  * lexemes' tables may be a spelling of others still (`vino`, of `venir`, is
  * also wine). Once checked, a spelling of no `lexemes` is one no source knows.
  */
final case class Homographs
  (
    spelling: Spelling,
    lexemes: List[LexemeId],
    checked: Map[Language, Timestamp],
  )
  derives Codec:

  /** These, with `lexemes` among them, each once. */
  def including(lexemes: List[LexemeId]): Homographs = copy(lexemes =
    (this.lexemes ++ lexemes).distinct,
  )

  /** These, checked in `language` at `at`. */
  def checkedIn(language: Language, at: Timestamp): Homographs =
    copy(checked = checked.updated(language, at))

object Homographs:

  /** None yet known of `spelling`, and none checked. */
  def none(spelling: Spelling): Homographs =
    Homographs(spelling, Nil, Map.empty)

/** A lexeme's own, so that homographs are apart. */
opaque type LexemeId = String

object LexemeId:

  /**
    * The lexeme spelt `lemma`, a `wordClass` of `language`, of the `etymology`
    * that a dictionary numbers it by among words spelt alike: by the whole
    * language, so that each of its varieties has the same, and the lemma last,
    * so that no word can be taken for another's.
    */
  def apply
    (
      language: Language,
      wordClass: WordClass,
      etymology: Int,
      lemma: Spelling,
    )
    : LexemeId = List(
    language.whole.code,
    wordClass,
    etymology.toString,
    lemma,
  ).mkString(":")

  extension (id: LexemeId)

    /** The lexeme's own as text. */
    def text: String = id

  given Codec[LexemeId] = Codec.from(
    Decoder.decodeString,
    Encoder.encodeString,
  )

/** A sense's own, by which a rating or a relation may name it. */
opaque type SenseId = String

object SenseId:

  /** The `index`th sense of the lexeme of `lexeme` that `source` gives. */
  def apply
    (
      lexeme: LexemeId,
      source: String,
      index: Int,
    )
    : SenseId = s"$lexeme#$source.$index"

  given Codec[SenseId] = Codec.from(
    Decoder.decodeString,
    Encoder.encodeString,
  )

/** A word class, as Wiktionary names it (`noun`, `verb`, `particle`). */
opaque type WordClass = String

object WordClass:

  def apply(name: String): WordClass = name

  extension (wordClass: WordClass)

    /** The word class as Wiktionary names it. */
    def name: String = wordClass

  given Codec[WordClass] = Codec.from(
    Decoder.decodeString,
    Encoder.encodeString,
  )

/**
  * A label from Wiktionary's vocabulary, into which every source's labels are
  * put: of grammar (`genitive`, `singular-possessive`), field (`computing`) or
  * register (`colloquial`).
  */
opaque type Tag = String

object Tag:

  def apply(name: String): Tag = name

  extension (tag: Tag)

    /** The tag as Wiktionary names it. */
    def name: String = tag

  given Codec[Tag] = Codec.from(
    Decoder.decodeString,
    Encoder.encodeString,
  )
