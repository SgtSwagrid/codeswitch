package com.alecdorrington.codeswitch

import io.circe.{Codec, Decoder, Encoder}
import scala.scalajs.js

/** A language that the reader may learn, with its ISO 639-1 `code`. */
enum Language(val code: String, val name: String):

  case Arabic extends Language("ar", "Arabic")

  case Chinese extends Language("zh", "Chinese")

  case Czech extends Language("cs", "Czech")

  case Danish extends Language("da", "Danish")

  case Dutch extends Language("nl", "Dutch")

  case English extends Language("en", "English")

  case Finnish extends Language("fi", "Finnish")

  case French extends Language("fr", "French")

  case German extends Language("de", "German")

  case Greek extends Language("el", "Greek")

  case Hebrew extends Language("he", "Hebrew")

  case Hindi extends Language("hi", "Hindi")

  case Indonesian extends Language("id", "Indonesian")

  case Italian extends Language("it", "Italian")

  case Japanese extends Language("ja", "Japanese")

  case Korean extends Language("ko", "Korean")

  case Norwegian extends Language("no", "Norwegian")

  case Polish extends Language("pl", "Polish")

  case Portuguese extends Language("pt", "Portuguese")

  case Russian extends Language("ru", "Russian")

  case Spanish extends Language("es", "Spanish")

  case Swedish extends Language("sv", "Swedish")

  case Turkish extends Language("tr", "Turkish")

  case Ukrainian extends Language("uk", "Ukrainian")

  case Vietnamese extends Language("vi", "Vietnamese")

object Language:

  /** The language whose code is `code`, if there is one. */
  def parse(code: String): Option[Language] = values.find(_.code == code)

  /**
    * The language of `tag`, as `de` or `de-AT`, if it is one of these, by its
    * code or by another that browsers and pages give for it.
    */
  def ofTag(tag: String): Option[Language] =
    val code = tag
      .trim
      .toLowerCase
      .takeWhile(char => char != '-' && char != '_')
    parse(aliases.getOrElse(code, code))

  /** Codes for some of these languages, older or narrower than their own. */
  private val aliases = Map(
    "iw" -> "he",
    "in" -> "id",
    "nb" -> "no",
    "nn" -> "no",
  )

  /** The language of the browser's own interface, if it is one of these. */
  def ofBrowser: Option[Language] = Option
    .when(js.typeOf(js.Dynamic.global.navigator) == "object")(
      js.Dynamic.global.navigator.language.asInstanceOf[js.UndefOr[String]],
    )
    .flatMap(_.toOption)
    .flatMap(ofTag)

  /**
    * A language, kept by its code. Not derived, which would keep it as an
    * object named for it instead, and lose the choices that readers have
    * already made.
    */
  given Codec[Language] = Codec.from(
    Decoder[String].emap(code =>
      parse(code).toRight(s"No language has the code $code."),
    ),
    Encoder[String].contramap(_.code),
  )
