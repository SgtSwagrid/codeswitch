package com.alecdorrington.codeswitch

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.alecdorrington.omnibrowser.{Browser, StorageKey, StorageKeys}
import fs2.Stream
import io.circe.Json
import scala.concurrent.duration.{Duration, FiniteDuration}

/**
  * Storage in memory, as the browser's, that a test may look into, which
  * refuses to keep what `refuses` names, and takes `latency` for each change,
  * so that changes may overlap.
  */
final class Memory private (
  values: Ref[IO, Map[String, Json]],
  refuses: String => Boolean,
  latency: FiniteDuration,
) extends Browser.Storage:

  def get[A](key: StorageKey[A]): IO[A] = pause *>
    values
      .get
      .flatMap(
        _.get(key.name)
          .fold(IO.pure(key.default))(json =>
            IO.fromEither(key.codec.decodeJson(json)),
          ),
      )

  def set[A](key: StorageKey[A], value: A): IO[Unit] = pause *>
    (if refuses(key.name) then IO.raiseError(Exception("The storage is full."))
     else values.update(_.updated(key.name, key.codec(value))))

  def remove[A](key: StorageKey[A]): IO[Unit] = values.update(_ - key.name)

  def changes[A](key: StorageKey[A]): Stream[IO, A] = Stream.empty

  def all[A](keys: StorageKeys[A]): IO[Map[String, A]] = values
    .get
    .flatMap(
      _.toList
        .flatMap((name, json) => keys.nameOf(name).map(_ -> json))
        .traverse((name, json) =>
          IO.fromEither(keys(name).codec.decodeJson(json)).tupleLeft(name),
        )
        .map(_.toMap),
    )

  /** The names of what is kept. */
  def names: IO[Set[String]] = values.get.map(_.keySet)

  /**
    * The time a change takes, if any, as even a sleep of none waits for the
    * next turn of JavaScript's event loop.
    */
  private def pause: IO[Unit] = IO.sleep(latency).whenA(latency > Duration.Zero)

object Memory:

  /**
    * Storage in memory, holding nothing yet, refusing what `refuses` names, and
    * taking `latency` for each change.
    */
  def apply
    (
      refuses: String => Boolean = _ => false,
      latency: FiniteDuration = Duration.Zero,
    )
    : IO[Memory] = Ref[IO]
    .of(Map.empty[String, Json])
    .map(new Memory(_, refuses, latency))
