package com.alecdorrington.codeswitch.services

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.alecdorrington.codeswitch.models.Spending
import com.alecdorrington.iris.{Conversation, Usd}
import com.alecdorrington.iris.prompt.{
  PromptEvent, PromptResponse, PromptService,
}
import com.alecdorrington.iris.query.query
import com.alecdorrington.omnibrowser.StorageArea
import fs2.Stream
import io.circe.Json
import munit.CatsEffectSuite
import scala.concurrent.duration.DurationInt

class SpendingSuite extends CatsEffectSuite:

  /**
    * A model that writes each of `replies` in turn, its text costing what is
    * given, if anything, a reply never ending if it is `endless`.
    */
  private def model
    (endless: Boolean = false)
    (replies: (String, Option[BigDecimal])*)
    : IO[PromptService[IO]] = Ref[IO]
    .of(replies.toList)
    .map[PromptService[IO]](left =>
      _ =>
        Stream
          .eval(left.modify(all => (all.drop(1), all.head)))
          .flatMap((text, cost) =>
            Stream.emit(PromptEvent.Written(PromptEvent.Part.Text, text)) ++
              (if endless then Stream.never[IO]
               else
                 Stream.emit(PromptEvent.Ended(
                   PromptResponse(text, cost = cost.map(Usd(_))),
                 ))),
          ),
    )

  test(
    "What has been spent is kept locally as a number of dollars, at first none.",
  ):
    assertEquals(
      (Spending.spent.area, Spending.spent.default),
      (StorageArea.Local, BigDecimal(0)),
    )
    assertEquals(
      Spending.spent.codec(BigDecimal("0.0123")),
      Json.fromBigDecimal(BigDecimal("0.0123")),
    )

  test("Charges whose replies end together are all counted."):
    val costs = (1 to 20).toList.map(cents => BigDecimal(cents) / 100)
    for
      storage <- Memory(latency = 1.milli)
      service = SpendingService(storage)
      _     <- costs.parTraverse_(cost => service.charge(Usd(cost)))
      spent <- storage.get(Spending.spent)
    yield assertEquals(spent, costs.sum)

  test("A charge that cannot be kept is ignored, not a failure."):
    for
      storage <- Memory(refuses = _ == Spending.spent.name)
      _       <- SpendingService(storage).charge(Usd(BigDecimal(1)))
      spent   <- storage.get(Spending.spent)
    yield assertEquals(spent, BigDecimal(0))

  test(
    "A client charges what each of its requests cost, every attempt of an ask, and nothing for a reply with no price.",
  ):
    for
      storage <- Memory()
      client  <- model()(
        ("Not JSON.", Some(BigDecimal("0.25"))),
        ("""["ok"]""", Some(BigDecimal("0.5"))),
        ("""["free"]""", None),
      )
      charged = SpendingService(storage).charging(client)
      first <- charged
        .query
        .send[List[String]](Conversation.userSays("Say ok."))
        .map(_.value)
      second <- charged
        .query
        .send[List[String]](Conversation.userSays("Say free."))
        .map(_.value)
      spent <- storage.get(Spending.spent)
    yield
      assertEquals(
        (first, second),
        (List("ok"), List("free")),
      )
      assertEquals(spent, BigDecimal("0.75"))

  test(
    "A streamed reply is charged once it ends, but not if it is cancelled first.",
  ):
    for
      storage <- Memory()
      ended   <- model()(("""["a", "b"]""", Some(BigDecimal("0.125"))))
      endless <-
        model(endless = true)(("""["a", "b"""", Some(BigDecimal("0.5"))))
      service = SpendingService(storage)
      answer <- service
        .charging(ended)
        .query
        .send[Stream[IO, String]](Conversation.userSays("Say a and b."))
      read <- answer.value.compile.toList
      _    <- answer.attempts
      cut  <- service
        .charging(endless)
        .query
        .send[Stream[IO, String]](Conversation.userSays("Say a and b."))
      first <- cut.value.take(1).compile.toList
      _     <- cut.cancel
      spent <- storage.get(Spending.spent)
    yield
      assertEquals(
        (read, first),
        (List("a", "b"), List("a")),
      )
      assertEquals(spent, BigDecimal("0.125"))

  test(
    "Every model offered has prices, so that no reply goes uncharged for want of them.",
  ):
    IO.realTime
      .map(now =>
        assertEquals(
          Translator.models.flatMap(_._2).filter(_.rates(now).isEmpty),
          Nil,
        ),
      )

  test(
    "What has been spent is shown in dollars to the cent, or as less than a cent.",
  ):
    assertEquals(
      List(
        "0",
        "0.004",
        "0.005",
        "0.01",
        "1.235",
        "12",
      ).map(amount => Spending.shown(BigDecimal(amount))),
      List(
        "US$0.00",
        "less than US$0.01",
        "less than US$0.01",
        "US$0.01",
        "US$1.24",
        "US$12.00",
      ),
    )
