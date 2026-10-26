package com.alecdorrington.codeswitch

import cats.effect.IO
import cats.effect.syntax.all.*
import cats.syntax.all.*
import fs2.Stream
import fs2.concurrent.Signal
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
    * @param intensities
    *   The share of the words to translate, then each new choice.
    * @param translate
    *   The translations of the given words into the chosen language.
    */
  def read
    (
      languages: Stream[IO, Option[Language]],
      intensities: Stream[IO, Double],
      translate: List[Word] => IO[List[Translation]],
    )
    : IO[Unit] = languages
    .zipWithIndex
    .switchMap((language, index) =>
      Stream.exec(IO(Highlighter.restore())) ++
        Stream
          .emits(language.toList)
          .flatMap(_ => pass(intensities, translate, index.toInt)),
    )
    .compile
    .drain

  /**
    * Translates words of the page, then of what is added to it, in `pass`, at
    * each intensity that `intensities` chooses in turn. Each change shows or
    * hides the translations whose draws it passes, and each rise translates
    * more of the page, until a translation fails.
    */
  private def pass
    (
      intensities: Stream[IO, Double],
      translate: List[Word] => IO[List[Translation]],
      pass: Int,
    )
    : Stream[IO, Unit] = Stream
    .resource(intensities.hold1Resource)
    .flatMap(intensity =>
      intensity
        .discrete
        .foreach(current => IO(Highlighter.reveal(current)))
        .concurrently(translating(intensity, translate, pass)),
    )

  /**
    * Translates words of the page, then of what is added to it, and of the page
    * again each time that `intensity` rises, until a translation fails.
    */
  private def translating
    (
      intensity: Signal[IO, Double],
      translate: List[Word] => IO[List[Translation]],
      pass: Int,
    )
    : Stream[IO, Unit] = Highlighter
    .nodes(rises(intensity))
    .evalMap(roots => translateAll(roots, intensity, translate, pass))
    .handleErrorWith(error =>
      Stream.exec(IO(console.error(s"Codeswitch: ${ error.getMessage }"))),
    )

  /** Each time that `intensity` rises. */
  private def rises(intensity: Signal[IO, Double]): Stream[IO, Unit] = intensity
    .discrete
    .zipWithPrevious
    .collect { case (Some(last), next) if next > last => () }

  private def translateAll
    (
      roots: List[Node],
      intensity: Signal[IO, Double],
      translate: List[Word] => IO[List[Translation]],
      pass: Int,
    )
    : IO[Unit] = intensity
    .get
    .flatMap(current =>
      IO(Words.batches(
        Highlighter.choose(roots, pass, current),
        batch,
      )),
    )
    .flatMap(_.parTraverseN(parallel)(translateBatch(_, intensity, translate)))
    .void

  /**
    * Translates `choices`, then shows each translation whose draw is below the
    * intensity as it is by then, hiding the rest.
    */
  private def translateBatch
    (
      choices: List[Choice],
      intensity: Signal[IO, Double],
      translate: List[Word] => IO[List[Translation]],
    )
    : IO[Unit] = translate(choices.map(_.word)).flatMap(translations =>
    IO(Highlighter.show(choices, translations)),
  ) *> intensity.get.flatMap(current => IO(Highlighter.reveal(current)))
