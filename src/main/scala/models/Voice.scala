package com.alecdorrington.codeswitch.models

import com.alecdorrington.megalingual.Language

/**
  * A voice of the browser's, which says words of the language of its BCP 47
  * `tag`, on the device itself if `local`, else by an online service of the
  * browser's maker.
  */
final case class Voice(tag: String, local: Boolean):

  /**
    * How far the voice is from how words of `language` are said, if it can say
    * them at all, as it is of a language alike: first, how many steps apart in
    * Megalingual's tree its language is from the variety that `language` is
    * spoken in (see [[Voice.spoken]]), then whether it is online, then its
    * language's place among Megalingual's.
    */
  def distance(language: Language): Option[(Int, Boolean, Int)] = Language
    .fromTag(tag)
    .filter(_.alike(language))
    .map(own => (Voice.steps(own, Voice.spoken(language)), !local, own.ordinal))

object Voice:

  /**
    * Of `voices`, the one that best says words of `language`, each a [[Voice]]
    * as `voice` gives it, the first of those as close: the nearest variety, one
    * on the device before one online. None if none is of a language alike, as a
    * voice of another would read the word as its own.
    */
  def best[V]
    (language: Language, voices: List[V])
    (voice: V => Voice)
    : Option[V] = voices
    .flatMap(each => voice(each).distance(language).map(_ -> each))
    .minByOption(_._1)
    .map(_._2)

  /**
    * The variety that words of `language` are said in: its first, as
    * Megalingual reads a broader language, German in German German and Chinese
    * in Simplified Chinese, else itself.
    */
  def spoken(language: Language): Language = language
    .children
    .headOption
    .fold(language)(spoken)

  /**
    * How many steps apart `from` and `to` are in Megalingual's tree, up from
    * `from` to the nearest language that both are within, then down to `to`.
    */
  private def steps(from: Language, to: Language): Int =
    val up = from.lineage.indexWhere(to.within)
    if up < 0 then Int.MaxValue else up + to.lineage.indexOf(from.lineage(up))
