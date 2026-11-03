package com.alecdorrington.codeswitch.views

import com.alecdorrington.codeswitch.models.{Familiarity, Standing}
import org.scalajs.dom.*

/**
  * A measure of how a word stands with the reader, called `name`, which a card
  * and the Vocabulary tab show alike (see [[Scores]]).
  */
enum Measure(val name: String):

  /** How well the reader knows the word, as a percentage. */
  case Familiarity extends Measure("Familiarity")

  /** How often the reader has met the word, as a count. */
  case Frequency extends Measure("Frequency")

  /** How much the reader should focus on learning the word, out of `100`. */
  case Priority extends Measure("Priority")

  /**
    * How full its bar is for a word that stands with the reader as `standing`
    * has it, from `0` to `1`: its familiarity and its priority as they are, and
    * how often it has been met against the average word of its language, two
    * thirds full for a word met as often, and nearly full for one met three
    * times as often, as its priority weighs it (see [[models.Priority.of]]).
    */
  def share(standing: Standing): Double = this match
    case Familiarity => standing.familiarity
    case Frequency   => -math.expm1(-standing.relative.max(0))
    case Priority    => standing.priority

  /** Its value for a word that stands with the reader as `standing` has it. */
  def shown(standing: Standing): String = this match
    case Familiarity => s"${ Scores.percent(standing.familiarity) }%"
    case Frequency   => math.round(standing.frequency).toString
    case Priority    => Scores.percent(standing.priority).toString

  /**
    * The colour of its bar when it is `share` full: familiarity's from red, for
    * a word the reader hardly knows, through amber, from which it is
    * [[models.Familiarity.familiar]], to teal, for one they know well;
    * frequency's blue; and priority's violet.
    */
  def colour(share: Double): Oklch = this match
    case Familiarity =>
      if share <= Scores.familiar then
        Oklch.red.towards(Oklch.amber, share / Scores.familiar)
      else
        Oklch
          .amber
          .towards(
            Oklch.teal,
            (share - Scores.familiar) / (1 - Scores.familiar),
          )
    case Frequency => Oklch.sky
    case Priority  => Oklch.violet

/**
  * A colour in OKLCH: its `lightness`, from `0` to `1`, its `chroma`, and its
  * `hue`, in degrees, so that a colour between two keeps theirs between theirs,
  * as one between red and teal goes through amber, not grey.
  */
final case class Oklch
  (
    lightness: Double,
    chroma: Double,
    hue: Double,
  ):

  /** The colour `part` of the way from this to `to`. */
  def towards(to: Oklch, part: Double): Oklch = Oklch(
    Scores.between(lightness, to.lightness, part),
    Scores.between(chroma, to.chroma, part),
    Scores.between(hue, to.hue, part),
  )

  /** This, as CSS writes it. */
  def css: String = List(lightness, chroma, hue)
    .map(Scores.rounded)
    .mkString("oklch(", " ", ")")

/**
  * The colours of the bars, each as bright on a light page as on a dark one:
  * the `500`s of Tailwind CSS, as it gives them.
  */
object Oklch:

  val red    = Oklch(0.637, 0.237, 25.331)
  val amber  = Oklch(0.769, 0.188, 70.08)
  val teal   = Oklch(0.704, 0.14, 182.503)
  val sky    = Oklch(0.685, 0.169, 237.323)
  val violet = Oklch(0.606, 0.25, 292.717)

/**
  * How a word stands with the reader, each [[Measure]] as a bar, which says
  * nothing to a reader of the screen, beside its value, the same in a card,
  * where each is named, and in the Vocabulary tab, where its column names it,
  * so that the two never differ.
  */
private[codeswitch] object Scores:

  /**
    * How a measure looks, wherever it is shown: its bar filled as far as its
    * share, `--share`, in its colour, `--colour`, as its element gives them, on
    * a track as light as the rest of the page is, or as dark.
    */
  val styles = """.score { --score-track: rgba(100, 116, 139, 0.22); display: inline-flex; align-items: center; gap: 6px; }
      |@media (prefers-color-scheme: dark) { .score { --score-track: rgba(148, 163, 184, 0.25); } }
      |.score .bar { flex: none; width: 40px; height: 6px; overflow: hidden; border-radius: 3px; background: var(--score-track); }
      |.score .bar > span { display: block; width: calc(var(--share) * 100%); height: 100%; border-radius: inherit; background: var(--colour); }
      |.score .value { min-width: 4ch; font-size: 12px; font-weight: 600; font-variant-numeric: tabular-nums; text-align: end; }
      |.score.named { display: grid; grid-template-columns: minmax(0, 1fr) auto; column-gap: 6px; row-gap: 3px; }
      |.score.named .name { grid-column: 1 / -1; font-size: 12px; opacity: 0.75; }
      |.score.named .bar { width: auto; }
      |""".stripMargin

  /**
    * Each measure of a word that stands with the reader as `standing` has it,
    * named.
    */
  def all(standing: Standing): List[HTMLElement] = Measure
    .values
    .toList
    .map(of(_, standing, named = true))

  /**
    * `measure` of a word that stands with the reader as `standing` has it: its
    * bar and its value, after its name, if `named`.
    */
  def of
    (
      measure: Measure,
      standing: Standing,
      named: Boolean,
    )
    : HTMLElement =
    val share = measure.share(standing).max(0).min(1)
    val score = Cards.element("span", "score").asInstanceOf[HTMLElement]
    score.classList.add(measure.toString.toLowerCase)
    score.classList.toggle("named", named)
    // Through the CSSOM, which no page's CSP blocks, as it may a style attribute.
    score.style.setProperty("--share", rounded(share).toString)
    score.style.setProperty("--colour", measure.colour(share).css)
    List(
      Option.when(named)(Cards.text("span", "name", measure.name)),
      Some(bar),
      Some(Cards.text(
        "span",
        "value",
        measure.shown(standing),
      )),
    ).flatten.foreach(score.appendChild)
    score

  /** A bar, filled as far as its score's share. */
  private def bar: Element =
    val bar = Cards.element("span", "bar")
    bar.setAttribute("aria-hidden", "true")
    bar.appendChild(document.createElement("span"))
    bar

  /** `share`, from `0` to `1`, as a whole percentage. */
  def percent(share: Double): Long = math.round(share * 100)

  /** The familiarity from which a word is [[models.Familiarity.familiar]]. */
  private[views] val familiar = Familiarity.familiar

  /** The value `part` of the way from `from` to `to`. */
  private[views] def between(from: Double, to: Double, part: Double): Double =
    from + (to - from) * part

  /** `value` to three decimal places, as CSS is given it. */
  private[views] def rounded(value: Double): Double = math.round(value * 1000) /
    1000.0
