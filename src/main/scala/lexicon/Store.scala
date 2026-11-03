package com.alecdorrington.codeswitch.lexicon

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import io.circe.{Decoder, Encoder}
import io.circe.parser.decode
import io.circe.syntax.*
import scala.scalajs.js

/**
  * Where the reader's lexicon is kept, on this device: its lexemes by their
  * own, the lexemes of each spelling, each record fetched, by its URL, and the
  * spellings read that wait to be looked up (see [[Pending]]), by their keys.
  */
trait Store:

  /** The lexemes of `ids` that are kept, in their order. */
  def lexemes(ids: List[LexemeId]): IO[List[Lexeme]]

  /** The lexemes spelt each of `spellings`, as kept, or none yet, in order. */
  def homographs(spellings: List[Spelling]): IO[List[Homographs]]

  /** The record fetched from `url`, if it is kept. */
  def record(url: String): IO[Option[Record]]

  /** Keeps `lexemes`, `homographs` and `records`, all of them or none. */
  def keep
    (
      lexemes: List[Lexeme],
      homographs: List[Homographs],
      records: List[Record],
    )
    : IO[Unit]

  /** The spellings that wait to be looked up, the first read first. */
  def waiting: IO[List[Pending]]

  /** The spellings of `keys` that wait, in order, each none if it does not. */
  def pending(keys: List[String]): IO[List[Option[Pending]]]

  /**
    * Keeps `waiting` as waiting, and takes out the spellings of the keys of
    * `done`, all of them or none.
    */
  def pend
    (
      waiting: List[Pending],
      done: List[String],
    )
    : IO[Unit]

object Store:

  /** A store in memory, which lasts only as long as the program. */
  def memory: IO[Store] = Ref[IO]
    .of(Values(
      Map.empty,
      Map.empty,
      Map.empty,
      Map.empty,
    ))
    .map(Memory(_))

  /**
    * The browser's IndexedDB, as a database named `name`, if the browser has
    * it, or else a store in memory, as a test in Node has none.
    */
  def browser(name: String): IO[Store] =
    if js.typeOf(js.Dynamic.global.indexedDB) == "undefined" then memory
    else Indexed.open(name)

  private final case class Values
    (
      lexemes: Map[LexemeId, Lexeme],
      homographs: Map[Spelling, Homographs],
      records: Map[String, Record],
      waiting: Map[String, Pending],
    )

  private final class Memory(values: Ref[IO, Values]) extends Store:

    def lexemes(ids: List[LexemeId]): IO[List[Lexeme]] = values
      .get
      .map(values => ids.flatMap(values.lexemes.get))

    def homographs(spellings: List[Spelling]): IO[List[Homographs]] = values
      .get
      .map(values =>
        spellings.map(spelling =>
          values.homographs.getOrElse(spelling, Homographs.none(spelling)),
        ),
      )

    def record(url: String): IO[Option[Record]] = values
      .get
      .map(_.records.get(url))

    def keep
      (
        lexemes: List[Lexeme],
        homographs: List[Homographs],
        records: List[Record],
      )
      : IO[Unit] = values.update(values =>
      values.copy(
        lexemes = values.lexemes ++ lexemes.map(lexeme => lexeme.id -> lexeme),
        homographs = values.homographs ++
          homographs.map(each => each.spelling -> each),
        records = values.records ++
          records.map(record => record.source.url -> record),
      ),
    )

    def waiting: IO[List[Pending]] = values
      .get
      .map(_.waiting.values.toList.sortBy(_.since.millis))

    def pending(keys: List[String]): IO[List[Option[Pending]]] = values
      .get
      .map(values => keys.map(values.waiting.get))

    def pend
      (
        waiting: List[Pending],
        done: List[String],
      )
      : IO[Unit] = values.update(values =>
      values.copy(waiting =
        values.waiting -- done ++ waiting.map(each => each.key -> each),
      ),
    )

  /**
    * A store in a database of IndexedDB, `database`, each value kept as JSON
    * under its key. Each read or write makes all its requests in one
    * transaction at once, as a transaction ends as soon as the browser turns to
    * another task with none of its requests waiting, as it would between the
    * steps of a program.
    */
  private final class Indexed(database: js.Dynamic) extends Store:

    def lexemes(ids: List[LexemeId]): IO[List[Lexeme]] =
      read[Lexeme](Indexed.lexemes, ids.map(_.text)).map(_.flatten)

    def homographs(spellings: List[Spelling]): IO[List[Homographs]] =
      read[Homographs](
        Indexed.homographs,
        spellings.map(_.text),
      ).map(
        _.zip(spellings)
          .map((kept, spelling) => kept.getOrElse(Homographs.none(spelling))),
      )

    def record(url: String): IO[Option[Record]] =
      read[Record](Indexed.records, List(url)).map(_.flatten.headOption)

    def waiting: IO[List[Pending]] = transaction(
      List(Indexed.pending),
      "readonly",
    )(stores =>
      val request = stores(Indexed.pending).getAll()
      () =>
        request
          .result
          .asInstanceOf[js.Array[String]]
          .toList
          .flatMap(decode[Pending](_).toOption)
          .sortBy(_.since.millis),
    )

    def pending(keys: List[String]): IO[List[Option[Pending]]] =
      read[Pending](Indexed.pending, keys)

    def pend
      (
        waiting: List[Pending],
        done: List[String],
      )
      : IO[Unit] = transaction(List(Indexed.pending), "readwrite")(stores =>
      done.foreach(stores(Indexed.pending).delete(_))
      waiting.foreach(each =>
        stores(Indexed.pending).put(each.asJson.noSpaces, each.key),
      )
      () => (),
    )

    def keep
      (
        lexemes: List[Lexeme],
        homographs: List[Homographs],
        records: List[Record],
      )
      : IO[Unit] = transaction(
      List(
        Indexed.lexemes,
        Indexed.homographs,
        Indexed.records,
      ),
      "readwrite",
    )(stores =>
      lexemes.foreach(lexeme =>
        stores(Indexed.lexemes).put(lexeme.asJson.noSpaces, lexeme.id.text),
      )
      homographs.foreach(each =>
        stores(Indexed.homographs).put(
          each.asJson.noSpaces,
          each.spelling.text,
        ),
      )
      records.foreach(record =>
        stores(Indexed.records).put(
          record.asJson.noSpaces,
          record.source.url,
        ),
      )
      () => (),
    )

    /**
      * The values kept under `keys` in the object store `name`, each none if
      * there is none, or if it cannot be read, as one an earlier version kept
      * otherwise.
      */
    private def read[A : Decoder]
      (name: String, keys: List[String])
      : IO[List[Option[A]]] = transaction(List(name), "readonly")(stores =>
      val requests = keys.map(stores(name).get(_))
      () =>
        requests.map(request =>
          Option(request.result.asInstanceOf[js.UndefOr[String]].orNull)
            .flatMap(decode[A](_).toOption),
        ),
    )

    /**
      * What `make` gives, once the transaction over the object stores `names`
      * in which it makes its requests completes: it makes them at once, giving
      * what reads their results then.
      */
    private def transaction[A]
      (names: List[String], mode: String)
      (make: Map[String, js.Dynamic] => () => A)
      : IO[A] = IO.async_[A](done =>
      try
        val transaction = database.transaction(js.Array(names*), mode)
        val result      =
          make(names.map(name => name -> transaction.objectStore(name)).toMap)
        transaction.oncomplete = (_: js.Any) => done(Right(result()))
        transaction.onerror = (_: js.Any) =>
          done(Left(Exception(s"The lexicon could not be kept: ${ transaction
              .error }")))
        transaction.onabort = (_: js.Any) =>
          done(Left(Exception("The lexicon's transaction was stopped.")))
      catch case error: Throwable => done(Left(error)),
    )

  private object Indexed:

    val lexemes    = "lexemes"
    val homographs = "homographs"
    val records    = "records"
    val pending    = "pending"

    /**
      * The database `name`, made with its object stores if it is new, or given
      * those it lacks if it is of an earlier version: the waiting spellings
      * came with version 2.
      */
    def open(name: String): IO[Store] = IO.async_[Store](done =>
      val request = js.Dynamic.global.indexedDB.open(name, 2)
      request.onupgradeneeded = (_: js.Any) =>
        val database = request.result
        List(lexemes, homographs, records, pending).foreach(store =>
          if !database.objectStoreNames.contains(store).asInstanceOf[Boolean]
          then database.createObjectStore(store),
        )
      request.onsuccess = (_: js.Any) => done(Right(Indexed(request.result)))
      request.onerror = (_: js.Any) =>
        done(Left(Exception(s"The lexicon could not be opened: ${ request
            .error }")))
      request.onblocked = (_: js.Any) =>
        done(Left(Exception("The lexicon is open elsewhere in an older form."))),
    )
