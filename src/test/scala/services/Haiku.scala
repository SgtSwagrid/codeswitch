package com.alecdorrington.codeswitch.services

import cats.effect.IO
import com.alecdorrington.iris.anthropic.{Anthropic, AnthropicModel}
import com.alecdorrington.iris.prompt.PromptService
import io.circe.Json
import io.circe.syntax.*
import sttp.client4.Backend

/**
  * Claude Haiku 4.5, as Code Switch prompts it, over a backend standing in for
  * Anthropic's API.
  */
object Haiku:

  /**
    * Anthropic's reply of `text`, ended as usual, as the server-sent events of
    * the stream that Iris asks for.
    */
  def replying(text: String): String = List(
    Json.obj(
      "type"    -> "message_start".asJson,
      "message" -> Json.obj(
        "usage" -> Json.obj(
          "input_tokens"  -> 10.asJson,
          "output_tokens" -> 5.asJson,
        ),
      ),
    ),
    Json.obj(
      "type"          -> "content_block_start".asJson,
      "index"         -> 0.asJson,
      "content_block" -> Json.obj("type" -> "text".asJson),
    ),
    Json.obj(
      "type"  -> "content_block_delta".asJson,
      "index" -> 0.asJson,
      "delta" -> Json.obj(
        "type" -> "text_delta".asJson,
        "text" -> text.asJson,
      ),
    ),
    Json.obj(
      "type"  -> "content_block_stop".asJson,
      "index" -> 0.asJson,
    ),
    Json.obj(
      "type"  -> "message_delta".asJson,
      "delta" -> Json.obj("stop_reason" -> "end_turn".asJson),
    ),
  ).map(event => s"data: ${ event.noSpaces }\n\n").mkString

  /**
    * The model over `backend`, which cannot stream, so that each request is
    * sent once, and its reply read whole.
    */
  def over(backend: Backend[IO]): PromptService[IO] = AnthropicModel
    .ClaudeHaiku4_5
    .prompt(
      Anthropic.api(Translator.access("key"), backend),
      Translator.settings,
    )
