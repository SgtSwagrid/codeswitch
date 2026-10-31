package com.alecdorrington.codeswitch

import com.alecdorrington.megalingual.Language
import com.alecdorrington.megalingual.Language.English
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/**
  * The words that the reader has rated or seen, of each language apart, as the
  * popup lists them.
  */
object Vocabulary:

  /**
    * The words of each language among `lexemes`, those being learnt first, in
    * the order of `learning`, then the rest by their English names, each sorted
    * as `sorting` says.
    */
  def byLanguage
    (
      lexemes: List[Lexeme],
      learning: List[Language],
      sorting: Sorting,
    )
    : List[(Language, List[Lexeme])] = lexemes
    .groupBy(_.language)
    .toList
    .sortBy((language, _) => (rank(learning, language), language.name(English)))
    .map((language, words) => language -> sorting(words))

  /**
    * Where `language` comes among `learning`, by its whole language, or after
    * all of them if it is not being learnt.
    */
  private def rank
    (
      learning: List[Language],
      language: Language,
    )
    : Int = Some(learning.indexWhere(_.alike(language)))
    .filter(_ >= 0)
    .getOrElse(learning.size)

/** A column of the tables of the words that the reader has rated or seen. */
enum Column(val name: String):

  case Word        extends Column("Word")
  case Familiarity extends Column("Familiarity")

  /** Whether it is sorted from its least first, until the reader turns it. */
  def ascendingFirst: Boolean = this == Word

/**
  * How the tables of the words that the reader has rated or seen are sorted: by
  * `column`, from its least first if `ascending`, or else from its greatest,
  * each tie by the word, then by its language.
  */
final case class Sorting(column: Column, ascending: Boolean):

  /** `words`, sorted so. */
  def apply(words: List[Lexeme]): List[Lexeme] = words.sorted(
    using (if ascending then primary else primary.reverse)
      .orElse(Sorting.byWord)
      .orElseBy(_.language.code),
  )

  /**
    * How the tables are sorted once the reader presses the heading of
    * `pressed`: the other way round, if they are sorted by it, or else by it,
    * as at first.
    */
  def by(pressed: Column): Sorting =
    if pressed == column then copy(ascending = !ascending)
    else Sorting.of(pressed)

  /** How `of` is sorted, as `aria-sort` names it. */
  def order(of: Column): String =
    if of != column then "none"
    else if ascending then "ascending"
    else "descending"

  private def primary: Ordering[Lexeme] = column match
    case Column.Word        => Sorting.byWord
    case Column.Familiarity => Ordering.by(_.familiarity)

object Sorting:

  /** As the tables are sorted at first: the best known first. */
  val initial: Sorting = of(Column.Familiarity)

  /** By `column`, as it is sorted at first. */
  def of(column: Column): Sorting = Sorting(column, column.ascendingFirst)

  /** Words in alphabetical order, as the browser's language orders them. */
  private val byWord: Ordering[Lexeme] =
    val collator = Collator()
    (one, other) => collator.compare(one.word, other.word)

/** Compares texts, by the rules of the locale. */
@js.native
@JSGlobal("Intl.Collator")
private class Collator extends js.Object:

  def compare(one: String, other: String): Int = js.native
