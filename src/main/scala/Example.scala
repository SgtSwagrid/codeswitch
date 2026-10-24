package com.alecdorrington

import com.alecdorrington.omnibrowser.*
import com.alecdorrington.omnibrowser.Permission.Storage

/** Counts the clicks of its toolbar button, and shows the count on it. */
object Example extends BrowserExtension[Storage]:

  override val name = "Example"

  override val description = "Counts the clicks of its toolbar button."

  override val icons = Seq(16, 32, 48, 128)
    .map(size => size -> s"icons/icon-$size.png")
    .toMap

  /** How many times the toolbar button has been clicked. */
  val clicks = StorageKey.local[Int]("clicks", 0)

  override val background = Background(onActionClicked((_, browser) =>
    browser
      .storage
      .update(clicks)(_ + 1)
      .flatMap(count => browser.action.setBadge(count.toString)),
  ))
