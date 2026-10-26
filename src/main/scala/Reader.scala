package com.alecdorrington.codeswitch

import cats.effect.{Deferred, IO, Ref}
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
    * when it changes, at each intensity that `intensities` chooses in turn.
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
    : IO[Unit] = (Ref[IO].of(0.0), Deferred[IO, Unit]).flatMapN(
    (intensity, known) =>
      follow(intensities, intensity, known)
        .both(
          languages
            .zipWithIndex
            .switchMap((language, index) =>
              Stream.exec(IO(Highlighter.restore())) ++
                Stream
                  .emits(language.toList)
                  .flatMap(_ =>
                    Stream.exec(known.get) ++
                      pass(intensity, translate, index.toInt),
                  ),
            )
            .compile
            .drain,
        )
        .void,
  )

  /**
    * Keeps each intensity that `intensities` chooses in `intensity`, completing
    * `known` with the first, and shows the translations whose draws are below
    * it, hiding the rest, at once.
    */
  private def follow
    (
      intensities: Stream[IO, Double],
      intensity: Ref[IO, Double],
      known: Deferred[IO, Unit],
    )
    : IO[Unit] = intensities
    .evalMap(current =>
      intensity.set(current) *> known.complete(()) *>
        IO(Highlighter.reveal(current)),
    )
    .compile
    .drain

  /**
    * Translates words of the page in `pass`, at `intensity`, then of what is
    * added to it, and of the page again each time that `intensity` rises, until
    * a translation fails.
    */
  private def pass
    (
      intensity: Ref[IO, Double],
      translate: List[Word] => IO[List[Translation]],
      pass: Int,
    )
    : Stream[IO, Unit] = Stream
    .eval(Ref[IO].of(Option.empty[Double]))
    .flatMap(last =>
      Highlighter
        .nodes(rose(intensity, last))
        .evalMap(roots => translateAll(roots, intensity, translate, pass)),
    )
    .handleErrorWith(error =>
      Stream.exec(IO(console.error(s"Codeswitch: ${ error.getMessage }"))),
    )

  /**
    * Whether the page is to be drawn up to `intensity`: first at once, and then
    * each time that it rises above `last`, as it was when last asked. While the
    * page is hidden, a rise waits until it is shown, so that the tabs in the
    * background do not all ask for translations at once.
    */
  private def rose
    (
      intensity: Ref[IO, Double],
      last: Ref[IO, Option[Double]],
    )
    : IO[Boolean] = (intensity.get, IO(Highlighter.outOfSight)).flatMapN(
    (current, hidden) =>
      last.modify:
        case None                     => (Some(current), current > 0)
        case Some(previous) if hidden => (Some(previous.min(current)), false)
        case Some(previous)           => (Some(current), current > previous),
  )

  private def translateAll
    (
      roots: List[Node],
      intensity: Ref[IO, Double],
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
    * intensity as it is by then, and hides the rest, again if the intensity
    * changed meanwhile.
    */
  private def translateBatch
    (
      choices: List[Choice],
      intensity: Ref[IO, Double],
      translate: List[Word] => IO[List[Translation]],
    )
    : IO[Unit] = translate(choices.map(_.word)).flatMap(translations =>
    intensity
      .get
      .flatMap(before =>
        IO(Highlighter.show(choices, translations, before)) *>
          intensity
            .get
            .flatMap(after =>
              IO(Highlighter.reveal(after)).whenA(after != before),
            ),
      ),
  )
