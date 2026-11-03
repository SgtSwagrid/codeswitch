package com.alecdorrington.codeswitch.views

import cats.effect.{IO, Resource}
import com.alecdorrington.annotatedtextformat.dom.{Focus, Hovering, Tooltip}
import com.alecdorrington.codeswitch.models.{
  Card, Glanced, Headword, Hint, Lookup, Marking, Rating, Standing,
}
import com.alecdorrington.omnibrowser.megalingual.Speech
import org.scalajs.dom.*

/**
  * The tooltips of the page, of the translation and of the hint under the
  * pointer, each the card of its word (see [[Cards]]): what it means there, the
  * original that a translation stands for, a hint's meaning, or the original of
  * a gloss, with a button for each [[models.Answer]] to whether the reader knew
  * it, and what the dictionary has of it. Each is shown above the line under
  * the pointer, or below it where there is no room above for a card filled in
  * ([[Cards.reserve]]), in the page's top layer, where nothing in the page can
  * clip or cover it, and stays open, on that side of the line, while the
  * pointer goes from the word to it. Each passage's hints, but those the reader
  * knows and those in links, are one stop of Tab, the left and right arrow keys
  * moving among them, each showing its tooltip, whose buttons Tab moves on to;
  * translations take no focus. A tap shows either, as hovering does, and a
  * second tap on a word in a link follows the link.
  *
  * A passage translated whole shows its original above it on hovering, in a
  * tooltip of its own, which the pointer passes through.
  *
  * @param translations
  *   What covers the translations.
  * @param hints
  *   What covers the hints.
  * @param renderings
  *   What covers the passages translated whole.
  * @param glances
  *   What watches the words of the translations and hints for what the reader
  *   does with them.
  * @param cards
  *   What makes the cards of their words.
  */
final class Tips private (
  val translations: Hovering[Translated],
  val hints: Hovering[Marking],
  val renderings: Hovering[Rendered],
  val glances: Glances,
  cards: Cards,
):

  /**
    * Covers no translation, hint or passage, closing each tooltip, watches no
    * word, and forgets the cards looked up, as the reader's languages may have
    * changed.
    */
  def clear(): Unit =
    translations.clear()
    hints.clear()
    renderings.clear()
    glances.clear()
    cards.forget()

  /**
    * Forgets the cards looked up, as the reader has changed their lexicon, so
    * that the next shows the change.
    */
  def forget(): Unit = cards.forget()

  /**
    * Takes the tooltips out of the page, and stops following the pointer and
    * watching words.
    */
  def stop(): Unit =
    List(translations, hints, renderings).foreach(hovering =>
      hovering.stop()
      hovering.tooltip.remove(),
    )
    glances.stop()

object Tips:

  /**
    * The original of a passage translated whole, in a card's colours, under a
    * band of the teal of its top, no wider than a column and cut short after a
    * dozen lines, as its tooltip cannot be scrolled.
    */
  private val originals =
    """:host { max-width: min(32rem, calc(100vw - 16px)) !important; padding: 10px 14px !important; border-top: 3px solid var(--head) !important; }
      |.original { display: -webkit-box; -webkit-box-orient: vertical; -webkit-line-clamp: 12; overflow: hidden; }"""
      .stripMargin

  /** The original passage of `rendered`, as its tooltip shows it. */
  private def original(rendered: Rendered): Node =
    val content = document.createElement("div")
    content.classList.add("original")
    content.textContent = rendered.original
    content

  /**
    * Shows the card of the word of each translation and hint pointed at, until
    * released, giving `rate` each answer for a word, opening its entry in the
    * reader's lexicon by `edit`, and giving `glance` what the reader does with
    * each word shown (see [[Glances]]), looking it up by `look`, finding how it
    * stands with the reader by `stand`, fetching its recording by `hear`,
    * saying it aloud by `speech`, and running programs by `run`.
    */
  def shown
    (
      rate: Rating => Unit,
      edit: String => Unit,
      glance: List[Glanced] => Unit,
      look: Lookup => IO[Either[String, Option[Card]]],
      stand: Headword => IO[Option[Standing]],
      hear: String => IO[String],
      speech: Speech,
      run: IO[Unit] => Unit,
    )
    : Resource[IO, Tips] = Resource.make(IO(Tips(
    Cards(rate, edit, look, stand, hear, speech, run),
    Glances(glance),
  )))(tips => IO(tips.stop()))

  private def apply(cards: Cards, glances: Glances): Tips = new Tips(
    carding(cards, glances, _.headword),
    carding(
      cards,
      glances,
      _.headword,
      Focus.Roving(_.kind != Hint.Kind.Known),
    ),
    Hovering(
      Tooltip(
        document,
        Cards.styles + originals,
        interactive = false,
        label = "Original",
      ),
      (hovered, _) => hovered.values.headOption.map(original),
      trusted = true,
    ),
    glances,
    cards,
  )

  /**
    * What shows, for the first annotation pointed at, if any is shown, the card
    * by `cards` of its word, as `headword` gives it, in a tooltip named for the
    * word, telling `glances` that it was looked at, the annotations that
    * `focus` gives taking the focus.
    */
  private def carding[A]
    (
      cards: Cards,
      glances: Glances,
      headword: A => Headword,
      focus: Focus[A] = Focus.Off,
    )
    : Hovering[A] =
    val tooltip = Tooltip(
      document,
      Cards.styles,
      label = "Meaning",
      reserve = Cards.reserve,
    )
    Hovering(
      tooltip,
      (hovered, showing) =>
        hovered
          .annotations
          .headOption
          .zip(hovered.values.headOption.map(headword))
          .map((index, word) =>
            glances.checked(hovered.decoration, index)
            tooltip.host.setAttribute("aria-label", Cards.label(word))
            cards(word, showing),
          ),
      focus,
      trusted = true,
    )
