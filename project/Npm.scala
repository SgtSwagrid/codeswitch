import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*
import sbt.*
import sbt.Keys.*
import sbt.internal.util.MessageOnlyException
import scala.sys.process.{Process, ProcessLogger}
import scala.util.Properties

/**
  * The packages from npm that the tests use, as `package.json` lists them, such
  * as jsdom, which gives the fake browser's pages a DOM.
  */
object Npm:

  val npmInstall = taskKey[Unit](
    "Installs the packages from npm that the tests use, unless they already are.",
  )

  /**
    * Installs the packages exactly as `package-lock.json` records them, unless
    * they were installed since it last changed. It is defined once for the
    * whole build, so that it never runs twice at once.
    */
  lazy val installation: Setting[?] = ThisBuild / npmInstall := Def.uncached {
    val root = (LocalRootProject / baseDirectory).value
    val log  = streams.value.log
    if !isCurrent(root) then
      log.info("Installing the packages from npm that the tests use.")
      val exit =
        Process(npm :+ "ci", root).!(ProcessLogger(log.info(_), log.info(_)))
      if exit != 0 then
        throw MessageOnlyException(
          s"npm failed to install the packages, with exit code $exit.",
        )
  }

  /** Installs the packages before the project's tests run. */
  lazy val beforeTests: Seq[Setting[?]] = Seq(
    Test / jsEnvInput :=
      Def.uncached((Test / jsEnvInput).dependsOn(ThisBuild / npmInstall).value),
  )

  /** Whether the packages were installed since `package-lock.json` changed. */
  private def isCurrent(root: File): Boolean =
    val installed = root / "node_modules" / ".package-lock.json"
    installed.exists && installed.lastModified >= (root / "package-lock.json")
      .lastModified

  /** npm, which Windows runs through its shell, as a script. */
  private val npm: Seq[String] =
    if Properties.isWin then Seq("cmd", "/c", "npm") else Seq("npm")
