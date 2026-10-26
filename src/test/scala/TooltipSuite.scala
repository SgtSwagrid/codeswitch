package com.alecdorrington.codeswitch

import munit.FunSuite

class TooltipSuite extends FunSuite:

  private val viewport = Box(0, 0, 1000, 800)

  private val tip = Box(0, 0, 80, 24)

  test("The original word is centred above its translation."):
    assertEquals(
      Tooltip.place(Box(400, 300, 40, 20), tip, viewport),
      Box(380, 270, 80, 24),
    )

  test("At the top of the window, the original word is below its translation."):
    assertEquals(
      Tooltip.place(Box(400, 10, 40, 20), tip, viewport),
      Box(380, 36, 80, 24),
    )

  test("At either side of the window, the original word stays within it."):
    assertEquals(
      Tooltip.place(Box(0, 300, 20, 20), tip, viewport).left,
      6.0,
    )
    assertEquals(
      Tooltip.place(Box(990, 300, 10, 20), tip, viewport).left,
      914.0,
    )
