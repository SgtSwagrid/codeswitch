package com.alecdorrington.codeswitch.services

import cats.effect.{Deferred, IO, Ref, SyncIO}
import cats.syntax.all.*
import scala.collection.immutable.SortedMap

/**
  * How soon a model's answer is wanted, the most urgent last: for the reader's
  * lexicon, in the background; for a page in a tab that is not shown; for the
  * page in a tab that is shown, as the reader reads it; and for what the reader
  * waits on, as a card or the editor.
  */
enum Urgency:

  case Background, Hidden, Shown, Waiting

/**
  * Runs programs that ask a model at most `lanes` at once, across every tab and
  * the lexicon, the most urgent first, and those as urgent in the order they
  * were asked for, so that what the reader waits on is not kept waiting behind
  * pages, nor the page they read behind those they do not, and the provider is
  * not asked more at once than its limits bear. A program cancelled while it
  * waits leaves the queue, as when its page is restored for another pass.
  */
final class Scheduler private (
  lanes: Int,
  state: Ref[IO, Scheduler.State],
):

  /** What `program` gives, run once a lane is free for it, as `urgency` says. */
  def apply[A](urgency: Urgency)(program: IO[A]): IO[A] = IO.uncancelable(
    poll =>
      Deferred[IO, Unit].flatMap(turn =>
        state
          .modify(_.ask(urgency, turn, lanes))
          .flatMap(place =>
            place.traverse_(place =>
              poll(turn.get).onCancel(state.flatModify(_.leaving(place, turn))),
            ),
          ) *> poll(program).guarantee(state.flatModify(_.done)),
      ),
  )

object Scheduler:

  /**
    * How many requests to the reader's model run at once, at most: enough that
    * a page's in sight and a card are asked for together, but not so many that
    * tabs loading together run into the provider's limits.
    */
  val lanes = 8

  /**
    * The one scheduler of every request to the reader's model, in the
    * background.
    */
  val models: Scheduler = apply(lanes).unsafeRunSync()

  /** A scheduler of `lanes` lanes, none yet taken. */
  def apply(lanes: Int): SyncIO[Scheduler] = Ref
    .in[SyncIO, IO, State](State(0, SortedMap.empty, 0))
    .map(new Scheduler(lanes, _))

  /**
    * Where a program waits: the more urgent first, then the earlier asked, by
    * the order of asking, `order`.
    */
  private type Place = (Int, Long)

  /**
    * How many programs run, `running`, those that wait for a lane, each with
    * what tells it it has one, and how many have been asked for, `asked`.
    */
  private final case class State
    (
      running: Int,
      waiting: SortedMap[Place, Deferred[IO, Unit]],
      asked: Long,
    ):

    /**
      * It, with a program of `urgency`, which `turn` tells that it may run,
      * running, if a lane of `lanes` is free and none waits, or else waiting,
      * where it waits.
      */
    def ask
      (
        urgency: Urgency,
        turn: Deferred[IO, Unit],
        lanes: Int,
      )
      : (State, Option[Place]) =
      if running < lanes && waiting.isEmpty then
        (copy(running = running + 1, asked = asked + 1), None)
      else
        val place = (-urgency.ordinal, asked)
        (
          copy(
            waiting = waiting.updated(place, turn),
            asked = asked + 1,
          ),
          Some(place),
        )

    /**
      * It, once the program waiting at `place` is cancelled: no longer waiting,
      * or, if it was given a lane meanwhile, with that lane handed on.
      */
    def leaving(place: Place, turn: Deferred[IO, Unit]): (State, IO[Unit]) =
      if waiting.get(place).contains(turn) then
        (copy(waiting = waiting - place), IO.unit)
      else done

    /**
      * It, once a program that ran is done: its lane handed to the first that
      * waits, if any, or else free.
      */
    def done: (State, IO[Unit]) = waiting.headOption match
      case Some((place, next)) =>
        (copy(waiting = waiting - place), next.complete(()).void)
      case None => (copy(running = running - 1), IO.unit)
