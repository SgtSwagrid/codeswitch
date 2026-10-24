package com.alecdorrington.codeswitch

import com.alecdorrington.omnibrowser.Codec

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

  /** A language, kept by its code. */
  given Codec[Language] = Codec[String].emap(code =>
    parse(code).toRight(s"No language has the code $code."),
  )(_.code)
