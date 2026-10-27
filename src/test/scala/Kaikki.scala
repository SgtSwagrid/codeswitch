package com.alecdorrington.codeswitch

import scala.scalajs.js

/**
  * Kaikki's entries, kept whole in `src/test/resources/kaikki`, as tests read
  * them.
  */
object Kaikki:

  /** The text of the entries kept in `name.jsonl`. */
  def lines(name: String): String = js
    .Dynamic
    .global
    .process
    .getBuiltinModule("node:fs")
    .readFileSync(
      s"${ js
          .Dynamic
          .global
          .process
          .cwd() }/src/test/resources/kaikki/$name.jsonl",
      "utf8",
    )
    .asInstanceOf[String]

  /** The entries kept in `name.jsonl`, as they would be read from Kaikki. */
  def entries(name: String): List[Entry] = Wiktionary.entries(
    lines(name),
    s"https://kaikki.org/$name",
  )
