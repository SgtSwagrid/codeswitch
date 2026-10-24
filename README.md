<div align="center">

  <h1>💬 Codeswitch</h1>
  <p>Teaches you a language by translating a few words of every page you read.</p>

  <span>
    <a href="https://github.com/SgtSwagrid/codeswitch/actions/workflows/build-integrity.yml"><img src="https://github.com/SgtSwagrid/codeswitch/actions/workflows/build-integrity.yml/badge.svg" alt="Build status" /></a>
  </span>

</div>

## ⬇️ Installation

Download the zip for your browser from the [latest release](https://github.com/SgtSwagrid/codeswitch/releases/latest), then:

- **Chrome:** unzip it, open `chrome://extensions`, turn on **Developer mode**, click **Load unpacked**, and choose the unzipped folder.
- **Firefox:** open `about:debugging#/runtime/this-firefox`, click **Load Temporary Add-on…**, and choose the zip.

## 💻 Local development

You'll need [sbt](https://www.scala-sbt.org/), [JDK 21](https://adoptium.net/) and [Node.js](https://nodejs.org/).

```bash
sbt omnibrowserBuild    # Builds the plugin, unpacked, for each browser.
sbt omnibrowserPackage  # Builds the plugin for release, zipped, for each browser.
sbt test                # Builds the plugin, then runs the tests.
```

The builds are written under `target/`, in `omnibrowser/development/<browser>` and `omnibrowser/release`.
Load a development build into the browser as above, choosing its folder (in Firefox, its `manifest.json`).

## 👁️ See also

- This plugin was made using [Scala Browser Plugin Template](https://github.com/SgtSwagrid/scala-browser-plugin-template).
