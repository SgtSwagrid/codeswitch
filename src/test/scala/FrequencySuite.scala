package com.alecdorrington.codeswitch

import cats.effect.IO
import cats.syntax.all.*
import com.alecdorrington.megalingual.Language
import munit.CatsEffectSuite
import scala.concurrent.duration.DurationInt

class FrequencySuite extends CatsEffectSuite:

  /** German, being learnt. */
  private val german = Language.German

  test(
    "A word never met has been met no times, and each word met is counted under its whole language, keeping its case, with the language's total.",
  ):
    for
      storage <- Memory()
      service = FrequencyService(storage)
      _ <- service.met(
        Language.SwissStandardGerman,
        List("Weg", "Weg", "weg"),
      )
      _ <- service.count(
        german,
        List("Bank" -> 2.5, " Weg " -> 1, "" -> 4),
      )
      counts <- service.of(
        german,
        List("Weg", "weg", "Bank", "Ufer"),
      )
      total <- service.total(Language.SwissStandardGerman)
      names <- storage.names
    yield
      assertEquals(
        counts,
        Map(
          "Weg"  -> 3.0,
          "weg"  -> 1.0,
          "Bank" -> 2.5,
          "Ufer" -> 0.0,
        ),
      )
      assertEquals(total, FrequencyTotal(6.5, 3))
      assertEquals(
        names,
        Set(
          "frequency:de:Weg",
          "frequency:de:weg",
          "frequency:de:Bank",
          "frequencyTotal:de",
        ),
      )

  test("Counts made at once lose none, and the total stays their sum."):
    for
      storage <- Memory(latency = 1.milli)
      service = FrequencyService(storage)
      _ <- List.fill(8)(service.met(german, List("Weg", "Bank"))).parSequence_
      counts <- service.of(german, List("Weg", "Bank"))
      total  <- service.total(german)
    yield
      assertEquals(counts, Map("Weg" -> 8.0, "Bank" -> 8.0))
      assertEquals(total, FrequencyTotal(16, 2))

  test(
    "A word switched is kept as the word that it was switched into, by its form in either case, which is counted as met as many times as the switch stands for.",
  ):
    val bank = Switched("Bank", Some(Level.A2))
    for
      storage <- Memory()
      service = FrequencyService(storage)
      _ <- service.switched(
        Language.English,
        List(
          Switch("Bank", bank, german, 20),
          Switch("bank", bank, german, 20),
          Switch("the", Switched(" ", None), german, 20),
        ),
      )
      switched <- service.switchedOf(
        Language.BritishEnglish,
        german,
        List("BANK", "the", "river"),
      )
      counts <- service.of(german, List("Bank"))
    yield
      assertEquals(switched, Map("BANK" -> bank))
      assertEquals(counts, Map("Bank" -> 40.0))

  test(
    "A switch stands for as many words as its word claims, within bounds, and a translation declined makes none.",
  ):
    val word = Word("bank", "the [[bank]]", german, 20)
    assertEquals(
      Switch.of(
        word,
        Translation("Bank", "Bank", "noun"),
      ),
      Some(Switch(
        "bank",
        Switched("Bank", None),
        german,
        20,
      )),
    )
    assertEquals(
      Switch.of(word, Translation.declined),
      None,
    )
    assertEquals(
      Switch
        .of(
          word.copy(times = 1e9),
          Translation("Bank", "Bank", "noun"),
        )
        .map(_.times),
      Some(Frequency.most),
    )
    assertEquals(Frequency.bounded(Double.NaN), 0.0)
    assertEquals(Frequency.bounded(-3), 0.0)

  test(
    "Each word's standing gives its priority from how well it is known and how often it is met against the average word, and a form switched before has the priority of the word it was switched into.",
  ):
    for
      storage <- Memory()
      frequency = FrequencyService(storage)
      priority  = PriorityService(
        FamiliarityService(
          storage,
          IO.pure(List(Learnt(german, Level.B1))),
        ),
        frequency,
      )
      _ <- frequency.count(german, List("Bank" -> 3, "Weg" -> 1))
      _ <- frequency.switched(
        Language.English,
        List(Switch(
          "bank",
          Switched("Bank", Some(Level.C1)),
          german,
          0,
        )),
      )
      standings <- priority.of(
        german,
        List(
          "Bank" -> Some(Level.C1),
          "Weg"  -> Some(Level.A1),
          "Ufer" -> None,
        ),
      )
      forms <- priority.ofForms(
        Language.English,
        german,
        List("bank", "way"),
      )
    yield
      assertEquals(
        standings,
        Map(
          "Bank" -> Standing(0.2, 3, 1.5),
          "Weg"  -> Standing(0.95, 1, 0.5),
          "Ufer" -> Standing(0.5, 0, 0),
        ),
      )
      assertEquals(forms.keySet, Set("bank"))
      assertEqualsDouble(
        forms("bank"),
        Priority.of(1.5, 0.2),
        1e-9,
      )
