<div align="center">

  <h1>💬 Codeswitch</h1>
  <p>Teaches you a language by translating a few words of every page you read.</p>

  <span>
    <a href="https://github.com/SgtSwagrid/codeswitch/actions/workflows/build-integrity.yml"><img src="https://github.com/SgtSwagrid/codeswitch/actions/workflows/build-integrity.yml/badge.svg" alt="Build status" /></a>
  </span>

</div>

## 🗣️ How it works

Choose the language you are learning from the toolbar button, and give it an API key from
[Anthropic](https://console.anthropic.com/), [OpenAI](https://platform.openai.com/api-keys) or
[Gemini](https://aistudio.google.com/apikey). From then on, about one word in twenty of every page you read
is shown in that language, highlighted. Hover over one to see the word it replaced.

- Each word is translated in its context (the passage around it, even when the word is a link or in italics),
  by a quick model of your key's provider (Claude Haiku 4.5, GPT-5 mini or Gemini 2.5 Flash-Lite), so it takes
  the sense and the form the sentence needs: a river's _bank_ is not a building society's.
- Text added to a page after it loads is translated too, as on pages that load more as you scroll.
- Code, text fields and anything a page marks not to translate are left alone.
- Choosing another language, or none, changes the pages already open at once.
- If translating fails, the toolbar button shows a `!`, and its popup says why.

Your key is kept in the browser on this device. The words chosen, each with up to a hundred characters of the text on
either side of it, are sent to your key's provider, and nowhere else; you pay the provider for them, which comes to a fraction
of a cent for most pages.

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
