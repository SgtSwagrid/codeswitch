package com.alecdorrington.codeswitch

import com.alecdorrington.megalingual.Language
import io.circe.parser.parse
import scala.scalajs.js

/**
  * Kaikki's entries, kept whole in `src/test/resources/kaikki`, as tests read
  * them.
  */
object Kaikki:

  /** The text of the entries kept in `name.jsonl`. */
  def lines(name: String): String = read(s"$name.jsonl")

  /** The names of the languages that Kaikki has dictionaries of. */
  def languages: Set[String] = read("languages.txt").linesIterator.toSet

  private def read(file: String): String = js
    .Dynamic
    .global
    .process
    .getBuiltinModule("node:fs")
    .readFileSync(
      s"${ js.Dynamic.global.process.cwd() }/src/test/resources/kaikki/$file",
      "utf8",
    )
    .asInstanceOf[String]

  /** The language of the entries kept in `name.jsonl`, as Kaikki codes it. */
  def language(name: String): Language = parse(lines(name).linesIterator.next())
    .flatMap(_.hcursor.get[String]("lang_code"))
    .toOption
    .flatMap(Language.fromCode)
    .get

  /** The entries kept in `name.jsonl`, as they would be read from Kaikki. */
  def entries(name: String): List[Entry] = Wiktionary.entries(
    language(name),
    lines(name),
    s"https://kaikki.org/$name",
  )
