package com.alecdorrington.codeswitch

import munit.FunSuite

class TooltipSuite extends FunSuite:

  private val viewport = Box(0, 0, 1000, 800)

  private val tip = Box(0, 0, 80, 24)

  test(
    "The original word is centred above its translation, or below it at the top of the window, and stays within the window at either side.",
  ):
    assertEquals(
      Tooltip.place(Box(400, 300, 40, 20), tip, viewport),
      Box(380, 270, 80, 24),
    )
    assertEquals(
      Tooltip.place(Box(400, 10, 40, 20), tip, viewport),
      Box(380, 36, 80, 24),
    )
    assertEquals(
      Tooltip.place(Box(0, 300, 20, 20), tip, viewport).left,
      6.0,
    )
    assertEquals(
      Tooltip.place(Box(990, 300, 10, 20), tip, viewport).left,
      914.0,
    )

  test("A hint's tooltip stays open on the way from its word to it."):
    val word  = Box(400, 300, 40, 20)
    val above = Tooltip.place(word, tip, viewport)
    assert(Tooltip.bridges(word, above, 420, 290))
    assert(Tooltip.bridges(word, above, 385, 275))
    assert(!Tooltip.bridges(word, above, 420, 330))
    assert(!Tooltip.bridges(word, above, 300, 290))
