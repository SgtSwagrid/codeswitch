package com.alecdorrington.codeswitch.views

import cats.effect.IO
import org.scalajs.dom.{
  window, AudioBuffer, AudioBufferSourceNode, AudioContext,
}
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}

/**
  * Plays speakers' recordings of words through the Web Audio API, from their
  * bytes, never from their addresses, as a page's content security policy may
  * forbid the page media from anywhere else, which would forbid an `<audio>`
  * element in it too.
  */
private object Recordings:

  /**
    * What plays recordings, made when the reader first asks to hear one, as a
    * browser lets a page play sound only once its reader has acted.
    */
  private var audio = Option.empty[AudioContext]

  /** The recording being played, if any. */
  private var playing = Option.empty[AudioBufferSourceNode]

  /**
    * Plays the recording that `recording` gives, its bytes in Base64, cutting
    * short the one being played, or fails if it cannot be had or played. What
    * plays it is readied at once, so run this as the reader acts.
    */
  def play(recording: IO[String]): IO[Unit] = IO(awake()).flatMap(context =>
    recording
      .flatMap(bytes =>
        IO.fromPromise(IO(context.decodeAudioData(decoded(bytes)))),
      )
      .flatMap(sound => IO(start(context, sound))),
  )

  /** Stops the recording being played, if any. */
  def stop(): Unit =
    playing.foreach(_.stop())
    playing = None

  /** What plays recordings, made, or let play again, if it was not. */
  private def awake(): AudioContext =
    val context = audio.getOrElse(AudioContext())
    audio = Some(context)
    if context.state == "suspended" then context.resume()
    context

  /** Plays `sound` in `context`, cutting short the one being played. */
  private def start
    (
      context: AudioContext,
      sound: AudioBuffer,
    )
    : Unit =
    stop()
    val source = context.createBufferSource()
    source.buffer = sound
    source.connect(context.destination)
    source.start()
    playing = Some(source)

  /** The bytes that `base64` holds. */
  private def decoded(base64: String): ArrayBuffer =
    val binary = window.atob(base64)
    val bytes  = Uint8Array(binary.length)
    binary.indices.foreach(index => bytes(index) = binary(index).toShort)
    bytes.buffer
