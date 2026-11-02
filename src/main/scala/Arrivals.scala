package com.alecdorrington.codeswitch

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import io.circe.{Codec, Decoder, Encoder}

/**
  * A `request`, known by `id`, the entries of whose reply the background sends
  * to the page that asked as soon as the model has written each, before the
  * whole reply.
  */
final case class Streamed[A](id: String, request: A)

object Streamed:

  // Not derived, as a derived codec needs a codec of `A`, which a list has not,
  // having an encoder and a decoder apart.
  given [A : {Encoder, Decoder}]: Codec[Streamed[A]] =
    Codec.forProduct2("id", "request")(Streamed.apply[A])(streamed =>
      (streamed.id, streamed.request),
    )

/** The entry at `index` of the reply to the request `id`, sent before the rest. */
final case class Arrival[A](id: String, index: Int, entry: A)

object Arrival:

  // Not derived, as `Streamed`'s is not.
  given [A : {Encoder, Decoder}]: Codec[Arrival[A]] =
    Codec.forProduct3("id", "index", "entry")(Arrival.apply[A])(arrival =>
      (arrival.id, arrival.index, arrival.entry),
    )

/**
  * The requests of a page in flight, by their ids, each with what to do with
  * each entry of its reply that arrives before the rest.
  */
final class Arrivals[A] private (
  waiting: Ref[IO, Map[String, Arrivals.Each[A]]],
):

  /**
    * What `request` gives, made with an id of its own, doing `each` with each
    * entry of its reply, by its index, that arrives meanwhile.
    */
  def asking[B](each: Arrivals.Each[A])(request: String => IO[B]): IO[B] = IO(
    Arrivals.id(),
  ).flatMap(id =>
    (waiting.update(
      _.updated(id, each),
    ) *> request(id)).guarantee(waiting.update(_ - id)),
  )

  /** Does with `arrival` what its request asked, if it is still in flight. */
  def arrived(arrival: Arrival[A]): IO[Unit] = waiting
    .get
    .flatMap(_.get(arrival.id).traverse_(_(arrival.index, arrival.entry)))

object Arrivals:

  /** What to do with each entry of a reply, by its index, as it arrives. */
  type Each[A] = (Int, A) => IO[Unit]

  /** Requests in flight, none yet. */
  def apply[A]: IO[Arrivals[A]] = Ref[IO]
    .of(Map.empty[String, Each[A]])
    .map(new Arrivals(_))

  /**
    * An id for a request, which no other of the tab's, even of the page before,
    * is likely to have.
    */
  private def id(): String =
    s"${ System.currentTimeMillis().toHexString }-${ (math.random() *
        Long.MaxValue).toLong.toHexString }"
