package com.alecdorrington.codeswitch.lexicon

import cats.effect.IO
import com.alecdorrington.omnibrowser.{Browser, BrowserExtension}
import com.alecdorrington.omnibrowser.testkit.FakeBrowser

/** An extension with no parts, installed only for its databases. */
object Bare extends BrowserExtension[Any]:

  override val name = "Bare"

/** Lexicons, and the extension's databases, for tests. */
object Stores:

  /**
    * The extension's databases, holding nothing yet, of a fake browser of their
    * own, which keeps them in memory, as Node has no IndexedDB.
    */
  def databases: IO[Browser.Databases] = FakeBrowser
    .install(Bare)
    .map(_.databases)

  /** A lexicon of its own, holding nothing yet, in [[databases]]. */
  def fresh: IO[Store] = databases.map(Store(_))

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
