package com.alecdorrington.codeswitch.views

import cats.effect.IO
import com.alecdorrington.codeswitch.models.Voice
import com.alecdorrington.megalingual.Language
import org.scalajs.dom.{
  window, AudioBuffer, AudioBufferSourceNode, AudioContext, Event,
  EventListenerOptions, EventTarget,
}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}

/**
  * Says words aloud: a speaker's recording of a word, where one is had, or else
  * the browser's own voice, which reads its spelling, as no browser's voices
  * read IPA. Each cuts short whatever was being said.
  *
  * A recording is played through the Web Audio API, from its bytes, never from
  * its address, as a page's content security policy may forbid the page media
  * from anywhere else, which would forbid an `<audio>` element in it too.
  */
private object Speech:

  /** The browser's speech, if it has any. */
  private def synthesis: Option[SpeechSynthesis] =
    val found = window.asInstanceOf[js.Dynamic].speechSynthesis
    Option.when(!js.isUndefined(found))(found.asInstanceOf[SpeechSynthesis])

  /**
    * The browser's voice that best says words of `language`, if it has one (see
    * [[models.Voice.best]]).
    */
  private def voice(language: Language): Option[SpeechSynthesisVoice] =
    synthesis.flatMap(synthesis =>
      Voice.best(language, synthesis.getVoices().toList)(voice =>
        Voice(voice.lang, voice.localService),
      ),
    )

  /**
    * Calls `found` once the browser has a voice for `language`: at once if it
    * has, or else once it has listed its voices, as Chrome lists none until
    * first asked; never if it has none.
    */
  def whenVoiced(language: Language)(found: () => Unit): Unit = synthesis
    .foreach(synthesis =>
      if voice(language).isDefined then found()
      else if synthesis.getVoices().isEmpty then
        synthesis.addEventListener(
          "voiceschanged",
          (_: Event) => if voice(language).isDefined then found(),
          new EventListenerOptions:
            once = true,
        ),
    )

  /**
    * Says `word`, of `language`, in the browser's voice for it, if it has one,
    * cutting short whatever was being said.
    */
  def say(word: String, language: Language): Unit = synthesis.foreach(
    synthesis =>
      voice(language).foreach(voice =>
        val utterance = SpeechSynthesisUtterance(word)
        utterance.voice = voice
        utterance.lang = voice.lang
        silence()
        synthesis.speak(utterance),
      ),
  )

  /**
    * What plays recordings, made when the reader first asks to hear one, as a
    * browser lets a page play sound only once its reader has acted.
    */
  private var audio = Option.empty[AudioContext]

  /** The recording being played, if any. */
  private var playing = Option.empty[AudioBufferSourceNode]

  /**
    * Plays the recording that `recording` gives, its bytes in Base64, cutting
    * short whatever was being said, or fails if it cannot be had or played.
    * What plays it is readied at once, so run this as the reader acts.
    */
  def play(recording: IO[String]): IO[Unit] = IO(awake()).flatMap(context =>
    recording
      .flatMap(bytes =>
        IO.fromPromise(IO(context.decodeAudioData(decoded(bytes)))),
      )
      .flatMap(sound => IO(start(context, sound))),
  )

  /** What plays recordings, made, or let play again, if it was not. */
  private def awake(): AudioContext =
    val context = audio.getOrElse(AudioContext())
    audio = Some(context)
    if context.state == "suspended" then context.resume()
    context

  /** Plays `sound` in `context`, cutting short whatever was being said. */
  private def start
    (
      context: AudioContext,
      sound: AudioBuffer,
    )
    : Unit =
    silence()
    val source = context.createBufferSource()
    source.buffer = sound
    source.connect(context.destination)
    source.start()
    playing = Some(source)

  /** Cuts short whatever was being said, in a voice or a recording. */
  private def silence(): Unit =
    synthesis.foreach(_.cancel())
    playing.foreach(_.stop())
    playing = None

  /** The bytes that `base64` holds. */
  private def decoded(base64: String): ArrayBuffer =
    val binary = window.atob(base64)
    val bytes  = Uint8Array(binary.length)
    binary.indices.foreach(index => bytes(index) = binary(index).toShort)
    bytes.buffer

/** The browser's speech, which says utterances aloud. */
@js.native
private trait SpeechSynthesis extends EventTarget:

  def getVoices(): js.Array[SpeechSynthesisVoice] = js.native

  def speak(utterance: SpeechSynthesisUtterance): Unit = js.native

  def cancel(): Unit = js.native

/**
  * A voice of the browser's, of the language of its BCP 47 tag, `lang`, on the
  * device itself if `localService`.
  */
@js.native
private trait SpeechSynthesisVoice extends js.Object:

  val lang: String = js.native

  val localService: Boolean = js.native

/** Words to be said aloud, `text`, in a `voice` and `lang`. */
@js.native
@JSGlobal
private class SpeechSynthesisUtterance(text: String) extends EventTarget:

  var voice: SpeechSynthesisVoice = js.native

  var lang: String = js.native
