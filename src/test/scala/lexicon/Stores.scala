package com.alecdorrington.codeswitch.lexicon

import cats.effect.IO
import com.alecdorrington.omnibrowser.BrowserExtension
import com.alecdorrington.omnibrowser.testkit.FakeBrowser

/** An extension with no parts, installed only for its databases. */
object Bare extends BrowserExtension[Any]:

  override val name = "Bare"

/** Lexicons for tests. */
object Stores:

  /**
    * A lexicon of its own, holding nothing yet, in the databases of a fake
    * browser, which keeps them in memory, as Node has no IndexedDB.
    */
  def fresh: IO[Store] = FakeBrowser
    .install(Bare)
    .map(browser => Store(browser.databases))

  /**
    * A lexicon of its own, as [[fresh]] gives, and what gives its databases
    * room to keep anything or none, as when the browser has none, failing each
    * write, which they have until it is told otherwise.
    */
  def refusing: IO[(Store, Boolean => IO[Unit])] = FakeBrowser
    .install(Bare)
    .map(browser =>
      Store(browser.databases) -> (room => browser.refuseWrites(!room)),
    )
