package com.alecdorrington.codeswitch

import cats.effect.{IO, Resource}
import com.alecdorrington.annotatedtextformat.dom.{Focus, Hovering, Tooltip}
import org.scalajs.dom.*

/**
  * The tooltips of the page: of the translation under the pointer, its original
  * text, and of the hint, the card of its word (see [[Cards]]): its meaning, or
  * the original of a gloss, with a button for each [[Answer]] to whether the
  * reader knew it, and what the dictionary has of it. Each is shown above the
  * line under the pointer, or below it where there is no room above, in the
  * page's top layer, where nothing in the page can clip or cover it. A hint's
  * stays open while the pointer goes from the word to it. Each passage's hints,
  * but those the reader knows and those in links, are one stop of Tab, the left
  * and right arrow keys moving among them, each showing its tooltip, whose
  * buttons Tab moves on to. A tap shows either, as hovering does, and a second
  * tap on a word in a link follows the link.
  *
  * @param translations
  *   What covers the translations.
  * @param hints
  *   What covers the hints.
  * @param cards
  *   What makes the cards of the hints' words.
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
    * How the tooltip of a translation looks, within its own shadow root, each
    * rule for its host `!important`, so that the page's own rules for every
    * element do not restyle it.
    */
  private val styles =
    """:host { padding: 3px 8px !important; border-radius: 6px !important; background: #0f766e !important; color: #fff !important; font: 600 13px/1.4 system-ui, sans-serif !important; white-space: nowrap !important; box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3) !important; }
      |""".stripMargin

  /**
    * Shows the original of each translation pointed at, and the card of each
    * hint's word, until released, giving `rate` each answer for a hint's word,
    * looking its word up by `look`, and running programs by `run`.
    */
  def shown
    (
      rate: Rating => Unit,
      look: Lookup => IO[Either[String, Option[Card]]],
      run: IO[Unit] => Unit,
    )
    : Resource[IO, Tips] =
    Resource.make(IO(Tips(Cards(rate, look, run))))(tips => IO(tips.stop()))

  private def apply(cards: Cards): Tips =
    val card = Tooltip(document, Cards.styles, label = "Meaning")
    new Tips(
      Hovering(
        Tooltip(
          document,
          styles,
          interactive = false,
          label = "Original",
        ),
        (hovered, _) =>
          hovered
            .values
            .headOption
            .map(translated => document.createTextNode(translated.original)),
        trusted = true,
      ),
      Hovering(
        card,
        (hovered, showing) =>
          hovered
            .values
            .headOption
            .map(marking =>
              card
                .host
                .setAttribute(
                  "aria-label",
                  Cards.label(marking.hint),
                )
              cards(marking, showing),
            ),
        Focus.Roving(_.kind != Hint.Kind.Known),
        trusted = true,
      ),
      cards,
    )
