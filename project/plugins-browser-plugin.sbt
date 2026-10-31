// SBT plugins common to all of my Scala browser plugins.
// Automatically synchronised from 'https://github.com/SgtSwagrid/scala-browser-plugin-config/'.

// For naming each version after the git tag it is released under (v1.2.3 is 1.2.3).
// https://github.com/sbt/sbt-dynver
addSbtPlugin("com.github.sbt" % "sbt-dynver" % "5.1.1")

// For writing browser plugins with Omnibrowser, and building them for Chrome and Firefox.
// It brings Scala.js, which compiles them to JavaScript, and adds only Omnibrowser's
// library to each plugin, at its own version (`omnibrowserVersion`). Each plugin adds
// the testkit, and Omnibrowser's other libraries, itself, at that version.
// https://github.com/SgtSwagrid/omnibrowser
addSbtPlugin("com.alecdorrington" % "sbt-omnibrowser" % "0.7.1")
