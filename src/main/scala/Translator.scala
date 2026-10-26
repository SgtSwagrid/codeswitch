package com.alecdorrington.codeswitch

import cats.MonadThrow
import cats.syntax.all.*
import com.alecdorrington.iris.*
import com.alecdorrington.omnibrowser.{Codec, StorageKey}
import io.circe.parser.decode
import io.circe.syntax.*
import scala.concurrent.duration.DurationInt

/** Something that keeps words from being translated, told to the reader. */
final case class Problem(message: String) extends Exception(message)

/**
  * Translates words in their contexts with a large language model, and keeps
  * the reader's choice of model, their keys, and what last went wrong. Only the
  * background and the popup use it, which keeps Iris's models out of the pages.
  */
object Translator:

  /** The model to translate with until the reader chooses another. */
  val defaultModel: LlmModel = LlmModel.ClaudeHaiku4_5

  /** The models that the reader may choose between, by their providers. */
  val models: List[(LlmProvider, List[LlmModel])] = LlmProvider
    .values
    .toList
    .map(provider =>
      provider -> LlmModel.Listed.values.toList.filter(_.provider == provider),
    )

  /** A model, kept by its name, as `anthropic:claude-haiku-4-5`. */
  given Codec[LlmModel] = Codec[String].emap(name =>
    LlmModel.parse(name).toRight(s"No model is named $name."),
  )(_.name)

  /** The model that translates, which names the provider that it is asked of. */
  val model = StorageKey.sync[LlmModel]("model", defaultModel)

  /** The reader's API keys, by the key of the provider each is for. */
  val apiKeys = StorageKey.local[Map[String, String]]("apiKeys", Map.empty)

  /** Why the last translation failed, or nothing if it did not. */
  val problem = StorageKey.local[String]("problem", "")

  /**
    * The configuration for `model`, with the reader's `apiKey` for its
    * provider. The token limit leaves a model that thinks first room to.
    */
  def config(model: LlmModel, apiKey: String): LlmConfig = LlmConfig(
    model,
    apiKey.trim,
    maxTokens = 8192,
    timeout = 1.minute,
  )

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
    val name = language.name
    s"""You help someone learn $name by translating a few words of the pages they read.
       |You are given a JSON array of pairs, each a word and its context, in which the word is marked as [[word]].
       |Reply with only a JSON array of strings, one for each word, in the same order: the word in $name, as it would be written in its place, in the sense it has there, and in the form the context needs.
       |Give a single word wherever one will do, and never more than three. Keep the word's capitalisation.
       |Give a name, a number, or a word already in $name as it is."""
      .stripMargin

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
