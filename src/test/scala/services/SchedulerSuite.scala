package com.alecdorrington.codeswitch.services

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import munit.CatsEffectSuite
import scala.concurrent.duration.DurationInt

class SchedulerSuite extends CatsEffectSuite:

  /** A moment for the fibers started to get as far as they can. */
  private val settle = IO.sleep(30.millis)

  /** A program that notes `name` as started, then waits for `gate`. */
  private def held
    (
      name: String,
      started: Ref[IO, List[String]],
      gate: Deferred[IO, Unit],
    )
    : IO[Unit] = started.update(_ :+ name) *> gate.get

  test(
    "At most as many programs run at once as there are lanes, the rest waiting until one is done.",
  ):
    for
      scheduler <- Scheduler(2).to[IO]
      started   <- Ref.of[IO, List[String]](Nil)
      gates     <- List.fill(3)(Deferred[IO, Unit]).sequence
      fibers    <- gates
        .zipWithIndex
        .traverse((gate, index) =>
          scheduler(Urgency.Shown)(held(s"$index", started, gate)).start <*
            settle,
        )
      first  <- started.get
      _      <- gates.head.complete(())
      _      <- settle
      second <- started.get
      _      <- gates.traverse_(_.complete(()))
      _      <- fibers.traverse_(_.joinWithNever)
    yield
      assertEquals(first, List("0", "1"))
      assertEquals(second, List("0", "1", "2"))

  test(
    "Of the programs waiting, the most urgent runs first, then the earlier asked for of those as urgent.",
  ):
    for
      scheduler <- Scheduler(1).to[IO]
      started   <- Ref.of[IO, List[String]](Nil)
      gate      <- Deferred[IO, Unit]
      blocker   <- scheduler(Urgency.Shown)(gate.get).start
      _         <- settle
      fibers    <- List(
        "hidden"      -> Urgency.Hidden,
        "shown"       -> Urgency.Shown,
        "background"  -> Urgency.Background,
        "waiting"     -> Urgency.Waiting,
        "shown again" -> Urgency.Shown,
      ).traverse((name, urgency) =>
        scheduler(urgency)(started.update(_ :+ name)).start <* settle,
      )
      _     <- gate.complete(())
      _     <- (blocker :: fibers).traverse_(_.joinWithNever)
      order <- started.get
    yield assertEquals(
      order,
      List(
        "waiting",
        "shown",
        "shown again",
        "hidden",
        "background",
      ),
    )

  test(
    "A program cancelled while it waits leaves the queue, and one cancelled while it runs, or that fails, frees its lane.",
  ):
    for
      scheduler <- Scheduler(1).to[IO]
      started   <- Ref.of[IO, List[String]](Nil)
      first     <- Deferred[IO, Unit]
      running   <- scheduler(Urgency.Shown)(held("first", started, first)).start
      _         <- settle
      dropped   <- scheduler(Urgency.Waiting)(started.update(_ :+ "dropped"))
        .start
      second <- Deferred[IO, Unit]
      next   <- scheduler(Urgency.Shown)(held("second", started, second)).start
      _      <- settle
      _      <- dropped.cancel
      _      <- first.complete(())
      _      <- settle
      _      <- next.cancel
      failed <- scheduler(Urgency.Shown)(IO.raiseError[Unit](Problem("No.")))
        .attempt
      last <-
        scheduler(Urgency.Shown)(started.update(_ :+ "last")).timeout(1.second)
      _     <- running.joinWithNever
      order <- started.get
    yield
      assertEquals(order, List("first", "second", "last"))
      assert(failed.isLeft)
