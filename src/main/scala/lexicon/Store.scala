package com.alecdorrington.codeswitch.lexicon

import cats.effect.IO
import com.alecdorrington.omnibrowser.{Browser, Database}

/**
  * Where the reader's lexicon is kept, on this device, in the extension's
  * `databases` (see [[Store.Kept]]): its lexemes by their own, the lexemes of
  * each spelling, each record fetched, by its URL, and the spellings read that
  * wait to be looked up (see [[Pending]]), by their keys. Each read or write is
  * one transaction, which sees every write before it whole.
  */
final class Store(databases: Browser.Databases):

  import Store.Kept

  /** The lexemes of `ids` that are kept, in their order. */
  def lexemes(ids: List[LexemeId]): IO[List[Lexeme]] = databases
    .get(Kept.lexemes, ids.map(_.text))
    .map(kept => ids.flatMap(id => kept.get(id.text)))

  /** The lexemes spelt each of `spellings`, as kept, or none yet, in order. */
  def homographs(spellings: List[Spelling]): IO[List[Homographs]] = databases
    .get(Kept.homographs, spellings.map(_.text))
    .map(kept =>
      spellings.map(spelling =>
        kept.getOrElse(
          spelling.text,
          Homographs.none(spelling),
        ),
      ),
    )

  /** The record fetched from `url`, if it is kept. */
  def record(url: String): IO[Option[Record]] = databases.get(Kept.records, url)

  /** Keeps `lexemes`, `homographs` and `records`, all of them or none. */
  def keep
    (
      lexemes: List[Lexeme],
      homographs: List[Homographs],
      records: List[Record],
    )
    : IO[Unit] = databases.write(
    lexemes.map(lexeme => Kept.lexemes.set(lexeme.id.text, lexeme)) ++
      homographs.map(each => Kept.homographs.set(each.spelling.text, each)) ++
      records.map(record => Kept.records.set(record.source.url, record))*,
  )

  /** The spellings that wait to be looked up, the first read first. */
  def waiting: IO[List[Pending]] = databases
    .all(Kept.pending)
    .map(_.values.toList.sortBy(_.since.millis))

  /** The spellings of `keys` that wait, in order, each none if it does not. */
  def pending(keys: List[String]): IO[List[Option[Pending]]] = databases
    .get(Kept.pending, keys)
    .map(kept => keys.map(kept.get))

  /**
    * Keeps `waiting` as waiting, and takes out the spellings of the keys of
    * `done`, all of them or none.
    */
  def pend
    (
      waiting: List[Pending],
      done: List[String],
    )
    : IO[Unit] = databases.write(
    done.map(Kept.pending.remove) ++
      waiting.map(each => Kept.pending.set(each.key, each))*,
  )

object Store:

  /**
    * The database in which the lexicon is kept, named apart from the one in
    * which earlier releases kept it otherwise (see [[forgotten]]).
    */
  private object Kept extends Database("words", version = 1):

    val lexemes = store[Lexeme]("lexemes")

    val homographs = store[Homographs]("homographs")

    val records = store[Record]("records")

    val pending = store[Pending]("pending")

  /**
    * The name of the database in which earlier releases kept the lexicon, each
    * value as text, which nothing reads now.
    */
  val forgotten = "lexicon"
