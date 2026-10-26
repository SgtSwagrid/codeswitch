package com.alecdorrington.codeswitch

import cats.effect.IO
import cats.effect.syntax.all.*
import cats.syntax.all.*
import fs2.Stream
import org.scalajs.dom.{console, Node}

/** Translates a share of the words of the page that the reader is reading. */
object Reader:

  /** How many words to ask for at once. */
  private val batch = 40

  /** How many requests to make at once. */
  private val parallel = 4

  /**
    * Translates words of the page, and of whatever is added to it, into each
    * language that `languages` chooses in turn, putting the original words back
    * when it changes.
    *
    * @param languages
    *   The language chosen, then each new choice, or `None` for none.
    * @param translate
    *   The translations of the given words into the chosen language.
    */
  def read
    (
      languages: Stream[IO, Option[Language]],
      translate: List[Word] => IO[List[Option[String]]],
    )
    : IO[Unit] = languages
    .zipWithIndex
    .switchMap((language, index) =>
      Stream.exec(IO(Highlighter.restore())) ++
        Stream.emits(language.toList).flatMap(_ => pass(translate, index.toInt)),
    )
    .compile
    .drain

  /**
    * Translates words of the page, then of what is added to it, in `pass`,
    * until a translation fails.
    */
  private def pass
    (
      translate: List[Word] => IO[List[Option[String]]],
      pass: Int,
    )
    : Stream[IO, Unit] = Highlighter
    .nodes
    .evalMap(roots => translateAll(roots, translate, pass))
    .handleErrorWith(error =>
      Stream.exec(IO(console.error(s"Codeswitch: ${ error.getMessage }"))),
    )

  private def translateAll
    (
      roots: List[Node],
      translate: List[Word] => IO[List[Option[String]]],
      pass: Int,
    )
    : IO[Unit] = IO(Words.batches(Highlighter.choose(roots, pass), batch))
    .flatMap(_.parTraverseN(parallel)(choices =>
      translate(choices.map(_.word)).flatMap(translations =>
        IO(Highlighter.show(choices, translations, pass)),
      ),
    ))
    .void
