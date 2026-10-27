<div align="center">

  <h1>💬 Codeswitch</h1>
  <p>Teaches you a language by translating a few words of every page you read.</p>

  <span>
    <a href="https://github.com/SgtSwagrid/codeswitch/actions/workflows/build-integrity.yml"><img src="https://github.com/SgtSwagrid/codeswitch/actions/workflows/build-integrity.yml/badge.svg" alt="Build status" /></a>
  </span>

</div>

## 🗣️ How it works

From the toolbar button, choose the language you are learning and the model to translate with, and give it
an API key from the model's provider: [Anthropic](https://console.anthropic.com/),
[OpenAI](https://platform.openai.com/api-keys) or [Gemini](https://aistudio.google.com/apikey). From then on:

- **Pages in other languages** have a share of their words (one in twenty, unless you choose more or fewer) shown
  in the language you are learning, highlighted. Hover over one to see the word it replaced.
- **Pages in the language you are learning** have the words a level above yours highlighted, with their meanings
  on hovering, and harder words and idioms shown as their meanings in your own language, in grey. Hover over one
  of those to see what it replaced. Choose your own language and your level (A1 to C2) in the popup.

Hovering over one of those, you can answer whether you knew it: **Knew it**, **Unsure** or **Didn't know**. Each answer
moves how well Codeswitch takes you to know the word (in any of its forms) a little that way, so a word you keep
knowing is soon only faintly underlined (where you can still say otherwise), and one you keep not knowing is shown as
its meaning. Only your first answer counts each time you hover. Your answers are kept on this device.

Each can be turned off on its own. Your browser tells which language each passage is in, on your device, or, where
it cannot tell (as Safari never can), the page does; a passage in no language that either can tell is taken to be in
another language.

- Each word is translated in its context (the passage around it, even when the word is a link or in italics),
  so it takes the sense and the form the sentence needs: a river's _bank_ is not a building society's.
- Where the language you are learning puts a word elsewhere in the sentence, it moves there, as a German verb
  goes to the end of its clause. A word may also take its article with it, or be split in two, as a separable
  verb is. Names, numbers and words that make no sense on their own are left as they are.
- Any model that [Iris](https://github.com/SgtSwagrid/iris) lists will do. Claude Haiku 4.5, the default, is
  quick and cheap, and enough for single words. Each provider's key is kept apart, so you can switch between
  models without typing a key again.
- Text added to a page after it loads is translated too, as on pages that load more as you scroll.
- Code, text fields and anything a page marks not to translate are left alone, and not sent.
- Changing a setting changes the pages already open at once.
- If translating fails, the toolbar button shows a `!`, and its popup says why.

Your keys are kept in the browser on this device. In pages in other languages, the words chosen, each with the text on
either side of it (at least a hundred characters, or the rest of its sentence, up to three hundred), are sent to the
chosen model's provider, and nowhere else. In pages in the language you are learning, the text of each passage is sent
there instead. You pay the provider for them, which with the default model comes to a fraction of a cent for most pages
in other languages, and a few cents for a long article in the language you are learning (more at lower levels, where more
words are marked). See the
[privacy policy](docs/PRIVACY_POLICY.md).

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
