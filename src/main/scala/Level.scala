package com.alecdorrington.codeswitch

import io.circe.{Codec, Decoder, Encoder}

/**
  * How well someone knows a language, or how hard a word of it is, on the
  * CEFR's scale.
  */
enum Level(val description: String):

  case A1 extends Level("beginner")

  case A2 extends Level("elementary")

  case B1 extends Level("intermediate")

  case B2 extends Level("upper intermediate")

  case C1 extends Level("advanced")

  case C2 extends Level("proficient")

object Level:

  /** The level named `name`, as `B2`, if there is one. */
  def parse(name: String): Option[Level] =
    values.find(_.toString == name.trim.toUpperCase)

  /** A level, kept by its name, as `B2`. */
  given Codec[Level] = Codec.from(
    Decoder[String].emap(name => parse(name).toRight(s"No level is $name.")),
    Encoder[String].contramap(_.toString),
  )
