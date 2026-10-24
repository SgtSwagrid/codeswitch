package com.alecdorrington.codeswitch

import cats.MonadThrow
import cats.syntax.all.*
import com.alecdorrington.iris.*
import io.circe.parser.decode
import io.circe.syntax.*
import scala.concurrent.duration.DurationInt

/** Something that keeps words from being translated, told to the reader. */
final case class Problem(message: String) extends Exception(message)

/** Translates words in their contexts with a large language model. */
object Translator:

  /**
    * The configuration for an API key, with a quick model of the provider that
    * the key belongs to, or `None` for a key of no provider known.
    */
  def config(apiKey: String): Option[LlmConfig] =
    model(apiKey.trim).map(LlmConfig(
      _,
      apiKey.trim,
      maxTokens = 2048,
      timeout = 1.minute,
    ))

  /** Why `apiKey` has no configuration. */
  def unusable(apiKey: String): Problem = Problem(
    if apiKey.trim.isEmpty then
      "Add an API key from Anthropic, OpenAI or Gemini."
    else "This API key is not from Anthropic, OpenAI or Gemini.",
  )

  private def model(apiKey: String): Option[LlmModel] = apiKey match
    case key if key.startsWith("sk-ant-") => Some(LlmModel.ClaudeHaiku4_5)
    case key if key.startsWith("sk-")     => Some(LlmModel.Gpt5Mini)
    case key if key.startsWith("AIza")    => Some(LlmModel.Gemini2_5FlashLite)
    case _                                => None

  /** The translations of `words` into `language`, by `client`. */
  def translate[F[_] : MonadThrow]
    (
      client: LlmClient[F],
      language: Language,
      words: List[Word],
    )
    : F[List[Option[String]]] = client
    .send(chat(language, words))
    .flatMap(reply => MonadThrow[F].fromEither(translations(reply.text, words)))

  /** The request to translate `words` into `language`. */
  def chat(language: Language, words: List[Word]): Chat = Chat()
    .withSystem(instructions(language))
    .user(words.map(word => word.text -> word.context).asJson.noSpaces)

  private def instructions(language: Language): String =
    s"""You help someone learn ${ language.name } by translating a few words of the pages they read.
       |You are given a JSON array of pairs, each a word and its context, in which the word is marked as [[word]].
       |Reply with only a JSON array of strings, one for each word, in the same order: the word in ${ language
        .name }, as it would be written in its place, in the sense it has there, and in the form the context needs.
       |Give a single word wherever one will do, and never more than three. Keep the word's capitalisation.
       |Give a name, a number, or a word already in ${ language
        .name } as it is.""".stripMargin

  /**
    * The translations of `words` in `reply`, each `None` if it is unchanged, or
    * a [[Problem]] if there are none.
    */
  def translations
    (reply: String, words: List[Word])
    : Either[Problem, List[Option[String]]] = decode[List[String]](array(reply))
    .leftMap(_ => Problem("The translations could not be read."))
    .map(found =>
      words.map(_.text).zip(found.map(Some(_)).padTo(words.size, None)),
    )
    .map(_.map((word, translation) => translation.filter(changed(word, _))))

  /** Why translating failed, as the reader is told. */
  def explain(error: Throwable): String = error match
    case Problem(message) => message
    case LlmError.Unsuccessful(provider, status, _, _)
      if status.code == 401 || status.code == 403 =>
      s"$provider refused the API key."
    case LlmError.Unsuccessful(provider, status, _, _) if status.code == 429 =>
      s"$provider is limiting requests. Try again soon."
    case LlmError.TimedOut(provider, _) => s"$provider took too long to answer."
    case other                          => other.getMessage

  /** The JSON array within `reply`, ignoring any words around it. */
  private def array(reply: String): String =
    val (start, end) = (reply.indexOf('['), reply.lastIndexOf(']'))
    if start >= 0 && end > start then reply.substring(start, end + 1) else reply

  private def changed(word: String, translation: String): Boolean =
    translation.trim.nonEmpty && !translation.trim.equalsIgnoreCase(word)
