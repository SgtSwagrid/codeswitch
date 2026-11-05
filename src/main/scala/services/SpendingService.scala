package com.alecdorrington.codeswitch.services

import cats.effect.{IO, SyncIO}
import cats.effect.std.Semaphore
import cats.syntax.all.*
import com.alecdorrington.codeswitch.models.Spending
import com.alecdorrington.iris.Usd
import com.alecdorrington.iris.prompt.PromptService
import com.alecdorrington.omnibrowser.Browser

/**
  * The one keeper of what the reader's requests to models have cost together,
  * in `storage` (see [[models.Spending]]). Any part may read it, but only the
  * background charges it, as it makes every request, one charge at a time, lest
  * two requests whose replies end together lose one.
  */
final class SpendingService(storage: Browser.Storage):

  /**
    * `client`, charging what each of its requests costs as its reply comes:
    * each attempt of an ask, as each is paid for, and a streamed reply once it
    * ends, but not one cancelled before it ends, which Iris does not price, nor
    * one of a model for which Iris lists no rates.
    */
  def charging(client: PromptService[IO]): PromptService[IO] =
    client.metered((_, cost) => cost.traverse_(charge))

  /**
    * Adds `cost` to what has been spent. A change that fails, as when storage
    * is full, is ignored, so that the request it is of still succeeds.
    */
  def charge(cost: Usd): IO[Unit] = SpendingService
    .lock
    .permit
    .surround(storage.update(Spending.spent)(_ + cost.amount))
    .void
    .handleError(_ => ())

object SpendingService:

  /**
    * What lets the background charge one cost at a time, lest two that overlap
    * lose one, as each reads the total before writing it. Six requests of each
    * kind go at once.
    */
  private lazy val lock = Semaphore.in[SyncIO, IO](1).unsafeRunSync()
