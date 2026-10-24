package com.alecdorrington

import com.alecdorrington.omnibrowser.testkit.FakeBrowser
import munit.CatsEffectSuite

class ExampleSuite extends CatsEffectSuite:

  test("Each click of the toolbar button is counted on its badge."):
    for
      browser <- FakeBrowser.install(Example)
      _       <- browser.clickAction
      _       <- browser.clickAction
      badge   <- browser.badge
      stored  <- browser.stored(Example.clicks)
    yield
      assertEquals(badge, "2")
      assertEquals(stored, 2)
