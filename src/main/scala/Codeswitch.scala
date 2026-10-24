package com.alecdorrington.codeswitch

import com.alecdorrington.omnibrowser.*

/** Teaches you a language by translating a few words of every page you read. */
object Codeswitch extends BrowserExtension[Any]:

  override val name = "Codeswitch"

  override val description =
    "Teaches you a language by translating a few words of every page you read."

  override val icons = Seq(16, 32, 48, 128)
    .map(size => size -> s"icons/icon-$size.png")
    .toMap
