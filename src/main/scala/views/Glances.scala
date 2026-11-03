package com.alecdorrington.codeswitch.views

import com.alecdorrington.annotatedtextformat.dom.Decoration
import com.alecdorrington.codeswitch.models.{
  Familiarity, Glance, Glanced, Headword,
}
import org.scalajs.dom.*
import scala.collection.mutable
import scala.scalajs.js

/**
  * Watches each word of a language being learnt that the page shows, as a hint
  * or a translation, for what the reader does with it, giving `glanced` each
  * time (see [[models.Glance]]): that it came into sight, as it first does
  * while the page is shown and it is not hidden, and that its card was looked
  * at, as it first is. Each word shown is watched apart, however often the page
  * shows it.
  */
final class Glances(glanced: List[Glanced] => Unit):

  /** The words watched, by their decorations. */
  private val watched = mutable.HashMap.empty[Decoration[?], List[Watched]]

  /** The words watched, by each of their elements in the page. */
  private val byElement = mutable.HashMap.empty[Element, List[Watched]]

  /** The words in sight not yet passed, as while hidden or out of sight. */
  private val waiting = mutable.LinkedHashSet.empty[Watched]

  private val observer = IntersectionObserver((entries, _) =>
    entries.foreach(entry =>
      byElement
        .getOrElse(entry.target, Nil)
        .foreach(sighted(_, entry.target, entry.isIntersecting)),
    )
    look(),
  )

  private val shown: js.Function1[Event, Unit] = _ => look()

  document.addEventListener("visibilitychange", shown)

  /**
    * Watches each word of `decoration` that `headword` gives one of, until the
    * decoration is forgotten.
    */
  def watch[A]
    (decoration: Decoration[A])
    (headword: A => Option[Headword])
    : Unit =
    val words = decoration
      .annotated
      .annotations
      .zipWithIndex
      .toList
      .filterNot((_, index) => decoration.unplaced(index))
      .flatMap((annotation, index) =>
        headword(annotation.value).map(Watched(
          decoration,
          index,
          _,
          decoration.elements(index),
        )),
      )
    watched.update(
      decoration,
      watched.getOrElse(decoration, Nil) ++ words,
    )
    words.foreach(word =>
      word
        .elements
        .foreach(element =>
          byElement.update(
            element,
            word :: byElement.getOrElse(element, Nil),
          )
          observer.observe(element),
        ),
    )

  /** Gives that the card of the word at `index` of `decoration` was looked at. */
  def checked(decoration: Decoration[?], index: Int): Unit = watched
    .getOrElse(decoration, Nil)
    .find(word => word.index == index && word.stage != Stage.Checked)
    .foreach(word =>
      waiting -= word
      send(List(word.glanced(Glance.Checked(word.stage == Stage.Passed))))
      word.stage = Stage.Checked,
    )

  /**
    * Gives that each word waiting came into sight, if the page is shown and the
    * word is not hidden.
    */
  def look(): Unit = if document.visibilityState == VisibilityState.visible then
    val passed = waiting.filter(_.isShown).toList
    waiting --= passed
    passed.foreach(_.stage = Stage.Passed)
    send(passed.map(_.glanced(Glance.Passed)))

  /** Stops watching the words of `decoration`. */
  def forget(decoration: Decoration[?]): Unit = watched
    .remove(decoration)
    .foreach(_.foreach(word =>
      waiting -= word
      word
        .elements
        .foreach(element =>
          observer.unobserve(element)
          byElement.remove(element),
        ),
    ))

  /** Stops watching every word. */
  def clear(): Unit = watched.keys.toList.foreach(forget)

  /** Stops watching every word, and the page. */
  def stop(): Unit =
    clear()
    observer.disconnect()
    document.removeEventListener("visibilitychange", shown)

  /**
    * Notes whether `element` of `word` is in sight, and so whether the word is
    * waiting to be passed.
    */
  private def sighted
    (
      word: Watched,
      element: Element,
      inSight: Boolean,
    )
    : Unit =
    if inSight then word.inSight += element else word.inSight -= element
    if word.stage == Stage.Unseen && word.inSight.nonEmpty then waiting += word
    else waiting -= word

  /** Gives `glances`, so many at a time as one message keeps. */
  private def send(glances: List[Glanced]): Unit = glances
    .grouped(Familiarity.most)
    .foreach(glanced)

/** How far the reader has gone with a word shown. */
private enum Stage:

  /** It has not come into sight, nor its card been looked at. */
  case Unseen

  /** It came into sight. */
  case Passed

  /** Its card was looked at. */
  case Checked

/**
  * The word at `index` of `decoration`, of `headword`, shown in `elements`, how
  * far the reader has gone with it, its `stage`, and which of its elements are
  * in sight.
  */
private final class Watched
  (
    val decoration: Decoration[?],
    val index: Int,
    headword: Headword,
    val elements: List[Element],
  ):

  var stage: Stage = Stage.Unseen

  val inSight: mutable.Set[Element] = mutable.Set.empty

  /** Whether it is shown, rather than hidden as the page's text. */
  def isShown: Boolean = decoration.isShown(index)

  /** That the reader did `glance` with it. */
  def glanced(glance: Glance): Glanced =
    Glanced(headword.word, headword.language, glance)
