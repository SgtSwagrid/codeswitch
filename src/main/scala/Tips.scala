package com.alecdorrington.codeswitch

import cats.effect.{IO, Resource}
import com.alecdorrington.annotatedtextformat.dom.{Focus, Hovering, Tooltip}
import org.scalajs.dom.*

/**
  * The tooltips of the page, of the translation and of the hint under the
  * pointer, each the card of its word (see [[Cards]]): what it means there, the
  * original that a translation stands for, a hint's meaning, or the original of
  * a gloss, with a button for each [[Answer]] to whether the reader knew it,
  * and what the dictionary has of it. Each is shown above the line under the
  * pointer, or below it where there is no room above, in the page's top layer,
  * where nothing in the page can clip or cover it, and stays open while the
  * pointer goes from the word to it. Each passage's hints, but those the reader
  * knows and those in links, are one stop of Tab, the left and right arrow keys
  * moving among them, each showing its tooltip, whose buttons Tab moves on to;
  * translations take no focus. A tap shows either, as hovering does, and a
  * second tap on a word in a link follows the link.
  *
  * @param translations
  *   What covers the translations.
  * @param hints
  *   What covers the hints.
  * @param cards
  *   What makes the cards of their words.
  */
final class Tips private (
  val translations: Hovering[Translated],
  val hints: Hovering[Marking],
  cards: Cards,
):

  /**
    * Covers no translation or hint, closing either tooltip, and forgets the
    * cards looked up, as the reader's languages may have changed.
    */
  def clear(): Unit =
    translations.clear()
    hints.clear()
    cards.forget()

  /** Takes both tooltips out of the page, and stops following the pointer. */
  def stop(): Unit = List(translations, hints).foreach(hovering =>
    hovering.stop()
    hovering.tooltip.remove(),
  )

object Tips:

  /**
    * Shows the card of the word of each translation and hint pointed at, until
    * released, giving `rate` each answer for a word, looking it up by `look`,
    * and running programs by `run`.
    */
  def shown
    (
      rate: Rating => Unit,
      look: Lookup => IO[Either[String, Option[Card]]],
      run: IO[Unit] => Unit,
    )
    : Resource[IO, Tips] =
    Resource.make(IO(Tips(Cards(rate, look, run))))(tips => IO(tips.stop()))

  private def apply(cards: Cards): Tips = new Tips(
    carding(cards, _.headword),
    carding(
      cards,
      _.headword,
      Focus.Roving(_.kind != Hint.Kind.Known),
    ),
    cards,
  )

  /**
    * What shows, for the first annotation pointed at, if any is shown, the card
    * by `cards` of its word, as `headword` gives it, in a tooltip named for the
    * word, the annotations that `focus` gives taking the focus.
    */
  private def carding[A]
    (
      cards: Cards,
      headword: A => Headword,
      focus: Focus[A] = Focus.Off,
    )
    : Hovering[A] =
    val tooltip = Tooltip(document, Cards.styles, label = "Meaning")
    Hovering(
      tooltip,
      (hovered, showing) =>
        hovered
          .values
          .headOption
          .map(headword)
          .map(word =>
            tooltip.host.setAttribute("aria-label", Cards.label(word))
            cards(word, showing),
          ),
      focus,
      trusted = true,
    )
