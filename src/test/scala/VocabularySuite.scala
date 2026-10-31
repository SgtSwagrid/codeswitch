package com.alecdorrington.codeswitch

import com.alecdorrington.megalingual.Language
import munit.FunSuite

class VocabularySuite extends FunSuite:

  private val words = List(
    Lexeme("Weg", Language.German, 0.5),
    Lexeme("Brücke", Language.German, 0.9),
    Lexeme("burro", Language.Italian, 0.5),
    Lexeme("Ärger", Language.German, 0.2),
    Lexeme("burro", Language.Spanish, 0.5),
  )

  private def named(lexemes: List[Lexeme]) =
    lexemes.map(lexeme => s"${ lexeme.word } (${ lexeme.language.code })")

  private def listed(sorting: Sorting) = named(sorting(words))

  test(
    "At first, the best known words come first, each tie in alphabetical order, then by language.",
  ):
    assertEquals(
      listed(Sorting.initial),
      List(
        "Brücke (de)",
        "burro (es)",
        "burro (it)",
        "Weg (de)",
        "Ärger (de)",
      ),
    )

  test(
    "Words sort in alphabetical order as the browser's language has it, regardless of case or accents, either way round, each tie still by language.",
  ):
    assertEquals(
      listed(Sorting.of(Column.Word)),
      List(
        "Ärger (de)",
        "Brücke (de)",
        "burro (es)",
        "burro (it)",
        "Weg (de)",
      ),
    )
    assertEquals(
      listed(Sorting(Column.Word, ascending = false)),
      List(
        "Weg (de)",
        "burro (es)",
        "burro (it)",
        "Brücke (de)",
        "Ärger (de)",
      ),
    )

  test(
    "Words sort by how often the reader has met them, or by their priority, the greatest first, so that one met often but known well comes last by priority.",
  ):
    val met = List(
      Lexeme(
        "Weg",
        Language.German,
        0.5,
        frequency = 3,
        relative = 1,
      ),
      Lexeme(
        "Bank",
        Language.German,
        0.2,
        frequency = 1,
        relative = 0.5,
      ),
      Lexeme(
        "Tor",
        Language.German,
        0.95,
        frequency = 12,
        relative = 4,
      ),
    )
    assertEquals(
      named(Sorting.of(Column.Frequency)(met)),
      List("Tor (de)", "Weg (de)", "Bank (de)"),
    )
    assertEquals(
      named(Sorting.of(Column.Priority)(met)),
      List("Bank (de)", "Weg (de)", "Tor (de)"),
    )

  test(
    "Pressing a column's heading sorts by it, as it is sorted at first, then the other way round, each press after.",
  ):
    val pressed = Sorting.initial.by(Column.Familiarity)
    assertEquals(
      pressed,
      Sorting(Column.Familiarity, ascending = true),
    )
    assertEquals(
      listed(pressed),
      List(
        "Ärger (de)",
        "burro (es)",
        "burro (it)",
        "Weg (de)",
        "Brücke (de)",
      ),
    )
    assertEquals(
      Sorting.initial.by(Column.Word),
      Sorting(Column.Word, ascending = true),
    )
    assertEquals(
      Sorting.initial.by(Column.Word).by(Column.Word),
      Sorting(Column.Word, ascending = false),
    )
    assertEquals(
      Column.values.toList.map(Sorting.initial.order),
      List("none", "descending", "none", "none"),
    )
    assertEquals(
      pressed.order(Column.Familiarity),
      "ascending",
    )

  test(
    "Words are listed by language, those being learnt first, as the reader orders them, by their whole languages, then the rest by name, each sorted alike.",
  ):
    val grouped = Vocabulary.byLanguage(
      words,
      List(
        Language.Spanish,
        Language.SwissStandardGerman,
      ),
      Sorting.initial,
    )
    assertEquals(
      grouped.map((language, lexemes) => language.code -> named(lexemes)),
      List(
        "es" -> List("burro (es)"),
        "de" -> List("Brücke (de)", "Weg (de)", "Ärger (de)"),
        "it" -> List("burro (it)"),
      ),
    )
