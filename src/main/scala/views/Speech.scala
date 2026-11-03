package com.alecdorrington.codeswitch.views

import com.alecdorrington.codeswitch.models.Voice
import com.alecdorrington.megalingual.Language
import org.scalajs.dom.{window, Event, EventListenerOptions, EventTarget}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal

/**
  * Says words aloud in the browser's own voices, which read their spelling, as
  * no browser's voices read IPA.
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
    * cutting short whatever it was saying.
    */
  def say(word: String, language: Language): Unit = synthesis.foreach(
    synthesis =>
      voice(language).foreach(voice =>
        val utterance = SpeechSynthesisUtterance(word)
        utterance.voice = voice
        utterance.lang = voice.lang
        synthesis.cancel()
        synthesis.speak(utterance),
      ),
  )

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
