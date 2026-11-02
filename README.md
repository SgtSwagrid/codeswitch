<div align="center">

  <h1>💬 Codeswitch</h1>
  <p>Teaches you a language by translating a few words of every page you read.</p>

  <span>
    <a href="https://github.com/SgtSwagrid/codeswitch/actions/workflows/build-integrity.yml"><img src="https://github.com/SgtSwagrid/codeswitch/actions/workflows/build-integrity.yml/badge.svg" alt="Build status" /></a>
    <a href="https://github.com/SgtSwagrid/codeswitch/releases/latest"><img src="https://img.shields.io/github/v/release/SgtSwagrid/codeswitch" alt="Latest release" /></a>
    <a href="https://addons.mozilla.org/firefox/addon/codeswitch/"><img src="https://img.shields.io/amo/v/codeswitch?label=firefox" alt="Firefox Add-ons version" /></a>
  </span>

</div>

## 🗣️ How it works

From the toolbar button, choose your native language (your browser's, until you choose another), any others you
read fluently, the languages you are learning, each with your level in it (A1 to C2), and the model to translate
with, and give it an API key from the model's provider: [Anthropic](https://console.anthropic.com/),
[OpenAI](https://platform.openai.com/api-keys) or [Gemini](https://aistudio.google.com/apikey). From then on:

- **Pages in your native language, or another you read fluently,** have a share of their words (one in twenty,
  unless you choose more or fewer) shown in the languages you are learning, each word in one of them, highlighted.
  Words you most need to learn are shown more often, and words you already know (as easy ones) seldom. Hover over one
  to see the word it replaced, and its dictionary card.
- **Pages in a language you are learning** have the words above your level in it that you most need to learn
  highlighted, with their meanings on hovering: those you meet often but do not yet know. Of the rest, those you know
  are faintly underlined, and the others shown as their meanings in your native language, in grey. Hover over one of
  those to see what it replaced.
- **Pages in any other language** are translated whole into your native language, keeping their links, with the
  original of each passage on hovering, unless you choose to leave them as they are. Their whole text is sent to
  your model's provider, which costs more than a share of their words.

Hovering over a word translated or marked, you can answer how well you knew it, from **1** (didn't know) to
**5** (knew it). Answering 1, 2 or 3 lowers how well Codeswitch takes you to know the word (in any of its forms), 1
the most, 4 leaves it be, and 5 raises it, a little each time, so that in pages in a language you are learning, a word
you keep knowing is soon only faintly underlined (where you can still say otherwise), and one you keep not knowing is
shown as its meaning. Until you answer, a word is taken to be known as its level for you puts it. Only your first
answer counts each time you hover. Your answers are kept on this device, as is how often you have met each word.

The popup's **Vocabulary** tab lists every word Codeswitch has shown you, each language in a table of its own, with
how well you know it, best known first, how often you have met it, and its priority, how much you need to learn it; choose a column's heading to sort by it instead, and hover over a word for
its card, where you can answer for it too.

Stay on one a moment and its dictionary entry fills in: how it is said, its principal forms (a noun's genitive and plural,
a verb's past and participles) and its first meanings. **More** adds every meaning with examples of its use, the tables of
its forms (for verbs of German, Spanish, Italian, Portuguese, French, Finnish and Russian, and nouns of German, Finnish,
Russian, Turkish and Polish) and where the word came from; each card after opens the same way until you choose **Less**.
The entries are [Wiktionary](https://www.wiktionary.org/)'s, by way of [Kaikki](https://kaikki.org/), or, for a word
Wiktionary lacks, written by your model. If your own language is not English, your model puts Wiktionary's English into
it. Each word is looked up once, then kept on this device.

Your browser tells which language each passage is in, on your device, or, where it cannot tell (as Safari never
can), the page does; a passage in no language that either can tell is taken to be in one you read fluently.

- Each word is translated in its context (the passage around it, even when the word is a link or in italics),
  so it takes the sense and the form the sentence needs: a river's _bank_ is not a building society's.
- Where a language you are learning puts a word elsewhere in the sentence, it moves there, as a German verb
  goes to the end of its clause. A word may also take its article with it, or be split in two, as a separable
  verb is. Names, numbers and words that make no sense on their own are left as they are.
- Any model that [Iris](https://github.com/SgtSwagrid/iris) lists will do. Claude Haiku 4.5, the default, is
  quick and cheap, and enough for single words. Each provider's key is kept apart, so you can switch between
  models without typing a key again.
- Text added to a page after it loads is translated too, as on pages that load more as you scroll.
- Code, text fields and anything a page marks not to translate are left alone, and not sent.
- Under **Websites**, choose whether Codeswitch runs on all websites except those you list, or only on those you list.
  Each choice keeps its own list. A website includes its subdomains, and a page includes the pages under it. You can
  add the site you are on in one click.
- Changing a setting changes the pages already open at once.
- If translating fails, the toolbar button shows a `!`, and its popup says why.

Your keys are kept in the browser on this device. In pages in languages you read fluently, the words chosen, each with the text on
either side of it (at least a hundred characters, or the rest of its sentence, up to three hundred), are sent to the
chosen model's provider, and nowhere else. In pages in a language you are learning, or in any other you have
translated, the text of each passage is sent there instead. You pay the provider for them, which with the default model comes to a fraction of a cent for most pages
in languages you read fluently, and a few cents for a long article in a language you are learning (more at lower levels, where more
words are marked). The words you look up are sent to Kaikki, and to the provider only where it writes or translates their
entries, which costs a fraction of a cent each. See the
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
