package com.alecdorrington.codeswitch

import com.alecdorrington.megalingual.Language
import io.circe.parser.parse
import scala.collection.mutable
import scala.scalajs.js

/**
  * Kaikki's entries, kept whole in `src/test/resources/kaikki`, as tests read
  * them. Each file is read and parsed once, however often tests ask for it.
  */
object Kaikki:

  /** The text of the entries kept in `name.jsonl`. */
  def lines(name: String): String = read(s"$name.jsonl")

  /** The names of the languages that Kaikki has dictionaries of. */
  def languages: Set[String] = read("languages.txt").linesIterator.toSet

  private def read(file: String): String = files.getOrElseUpdate(
    file,
    js.Dynamic
      .global
      .process
      .getBuiltinModule("node:fs")
      .readFileSync(
        s"${ js.Dynamic.global.process.cwd() }/src/test/resources/kaikki/$file",
        "utf8",
      )
      .asInstanceOf[String],
  )

  /** The language of the entries kept in `name.jsonl`, as Kaikki codes it. */
  def language(name: String): Language = languagesOf.getOrElseUpdate(
    name,
    parse(lines(name).linesIterator.next())
      .flatMap(_.hcursor.get[String]("lang_code"))
      .toOption
      .flatMap(Language.fromCode)
      .get,
  )

  /** The entries kept in `name.jsonl`, as they would be read from Kaikki. */
  def entries(name: String): List[Entry] = entriesOf.getOrElseUpdate(
    name,
    Wiktionary.entries(
      language(name),
      lines(name),
      s"https://kaikki.org/$name",
    ),
  )

  // What has been read and parsed already, as the tests read the same files
  // often, which is slow in JavaScript.
  private val files       = mutable.Map.empty[String, String]
  private val languagesOf = mutable.Map.empty[String, Language]
  private val entriesOf   = mutable.Map.empty[String, List[Entry]]
