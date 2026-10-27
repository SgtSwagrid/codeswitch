package com.alecdorrington.codeswitch

import cats.effect.IO
import cats.syntax.all.*
import com.alecdorrington.iris.LlmClient
import com.alecdorrington.omnibrowser.StorageKey
import org.scalajs.dom

/**
  * Looks words up for the card: in Wiktionary, through Kaikki, wherever it has
  * them, and otherwise from a model. Only the background uses it, which keeps
  * Iris out of the pages.
  */
object Dictionary:

  /**
    * Runs a program with a client of the reader's model, asked for only when a
    * model is needed, as it needs the reader's key.
    */
  type Ask = [A] => (LlmClient[IO] => IO[A]) => IO[A]

  /**
    * Where the entry for `lookup`, for `learner`, is kept once found, on this
    * device: by the language learnt, the reader's own, in which it explains,
    * the part of speech, and the word, in its own case.
    */
  def key(learner: Learner, lookup: Lookup): StorageKey[Option[Entry]] =
    StorageKey.local[Option[Entry]](
      List(
        "entry",
        learner.language.code,
        learner.native.code,
        lookup.pos,
        lookup.word.trim,
      ).mkString(":"),
      None,
    )

  /**
    * The entry for `lookup`, a word of the language that `learner` is learning,
    * explained in their own: Wiktionary's, from `fetch`, its glosses put into
    * their language by a model if that is not English; or else, if Wiktionary
    * has none, one that a model makes; or none, if neither can be had.
    */
  def entry
    (
      learner: Learner,
      lookup: Lookup,
      fetch: String => IO[Option[String]],
      ask: Ask,
    )
    : IO[Option[Entry]] =
    val url = Wiktionary.url(learner.language, lookup.word.trim)
    fetch(url)
      .map(_.flatMap(lines =>
        Wiktionary.choose(Wiktionary.entries(lines, url), lookup),
      ))
      .flatMap {
        case Some(entry) if learner.native == Language.English =>
          IO.pure(Some(entry))
        case Some(entry) =>
          ask(Translator.explained(_, learner, entry)).map(Some(_))
        case None => ask(Translator.defined(_, learner, lookup))
      }

  /**
    * The text at `url`, if it is there, through the browser, which the
    * background may ask of Kaikki, as `hosts` grants it, though Kaikki does not
    * allow requests from other origins. Any failure is as though it were not.
    */
  def fetch(url: String): IO[Option[String]] = IO
    .fromPromise(IO(dom.fetch(url)))
    .flatMap(response =>
      if response.ok then IO.fromPromise(IO(response.text())).map(Some(_))
      else IO.pure(None),
    )
    .handleError(_ => None)
