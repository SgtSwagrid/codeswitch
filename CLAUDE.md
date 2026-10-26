# CLAUDE.md

This file provides guidance to [Claude Code](https://claude.com/product/claude-code) when working with code in this repository.
It is not intended for human eyes.

### Maintenance

You (robot or human) have standing permission to update this file without asking.
Add important patterns, gotchas, or context that would help future sessions.
Keep it concise and actionable.

## Project overview

This is a Scala browser plugin that teaches you a language by translating a few words of every page you read.

- It is written with [Omnibrowser](https://github.com/SgtSwagrid/omnibrowser): one object extending `BrowserExtension` defines the plugin, and the `sbt-omnibrowser` plugin (added by the synced `project/plugins-browser-plugin.sbt`) compiles it to JavaScript with Scala.js and lays it out for Chrome and Firefox.
- `sbt omnibrowserBuild` builds it unpacked, for development, under `target/**/omnibrowser/development/<browser>/`, which a browser can load as it is.
  `sbt omnibrowserPackage` zips it for release, under `target/**/omnibrowser/release/`.
- Files in `src/main/extension`, such as icons, are copied into the plugin as they are.
- Versions come from git tags (`v1.2.3` is `1.2.3`), by sbt-dynver. Publishing a GitHub release packages it and attaches the zips (`publish.yml`).

### Architecture

- `Codeswitch` wires the parts, as methods (`background()`, `contentScripts()`, `popup()`): the storage keys (`language` and `intensity` in sync storage; `apiKeys`, keyed by `LlmProvider.key`, in local; `problem` in session, as it explains a badge that doesn't outlive the browser), the `translate` endpoint, the background that answers it, the content script and the popup. Only `model` (sync) is on `Translator`, beside the default and codec it is built from.
- Pages load their own bundle, `omnibrowser-content.js`, holding only what `contentScripts()` and `Codeswitch`'s own values reach; the main `omnibrowser.js` is for the background and popup. So `Codeswitch`'s values must not reach Iris or `Translator` (`model`'s codec and default reach Iris's model lists, and `hosts` is a `def` for `LlmProvider`): put what only the background or popup needs in their methods or on `Translator`. The release content bundle is about 2.75 MB (26 KB of it the intensity's own code), against 3.89 MB when pages loaded the main bundle; `grep -o 'com\.alecdorrington\.iris\.[A-Za-z]*'` on it should find nothing.
- The content script (`Reader`) watches the chosen language and, for each choice in turn (`switchMap`), restores the page, then translates its body and every node added to it, a pass at a time. `Highlighter` does all DOM work: it chooses words in text nodes (marking each node with the pass and the intensity its words were drawn up to, which each text that `split` takes off it copies, so that its words are not drawn again), and shows each word's translation as its model laid it out (below), each piece a `span.codeswitch` holding the original in `data-codeswitch`, styled by `src/main/extension/content.css` (put in every page by the manifest, through `withCss`).
- A translation (`Translation`, in `Placement.scala`) is a list of `Piece`s, each with a `Place`: the word's own (`Home`), a run of up to `Placement.reach` words around it (`Replacing`, as an article with its noun), or beside a quoted anchor (`After`/`Before`, as a German verb at the end of its clause); several pieces split a word (a separable verb), and none declines it. `Placement.lay` (pure, tested in Node) finds the places in the passage's text: a run within the word's own text node, of at most `reach` words split at spaces and with no digits; an anchor's nearest occurrence within the word's sentence, as a whole word (`Words.occurrences`: not part of a word joined by a hyphen or apostrophe, either apostrophe for either). Any piece without a place declines the whole translation, and so does one that would empty a link, button or label (`Highlighter.empties`).
- If no piece stays in the word's place, its place becomes an empty `span.codeswitch-gap`, and so does the space beside it (perhaps in the next text node, as for a word alone in `<em>`; never a line break, which `pre-wrap` keeps), so that no two spaces are left together. A moved piece is a `span.codeswitch-inserted` holding the space and its `span.codeswitch`. `restore()` removes the inserted spans, then puts each gap's and translation's `data-codeswitch` back, which must give the page's text exactly.
- `Choice` keeps its word as a live DOM `Range`, which follows the word as other translations split its text node, and is checked against the word before showing. A live range that begins where an element is inserted keeps beginning at the end of the text before it, so the element falls within it: `wordOf` first moves such a start to the next text with prose (`settle`).
- `show` takes each batch's choices from the last back, each in a `Try`. It lays each translation out in its `Passage` as it is then: the passage's prose texts, and each element within `skipped` (ours included) as one U+FFFC, so that a space beside one of our spans is not taken as beside the word. It finds each edit's text and offsets before making any, and makes them from the end of the passage back, a replacement before an insertion at the same place. The pieces put in one place go in as one `DocumentFragment`, as separate insertions at a node's start would come out reversed.
- Edits split a text node only within it, never at its ends; a replacement at a node's start leaves the page's node itself, empty, after the element, as a page or framework may hold on to it. Each text split off, each element put in a text's place, and the text that restore puts back for it are marked with the page's text they came from (`codeswitchOrigin`), and `restore()` joins each run of them that is still side by side back into it, so the page keeps its own text nodes, whole, even if a page script has since put something between them.
- The intensity (0 to 40%, 5% by default, from the popup's slider) is the share of words translated. Each word is drawn a number from 0 to 1 and is translated while the intensity is above it, so changing it never redraws the page:
  - A rise draws each text's remaining words again from its mark's level up (`Words.choose`'s `floor`), keeping those shown. `Reader` keeps the intensity in a cats-effect `Ref`, and `Highlighter.nodes` rescans the body on the next one-second tick after a rise, but only once the page is shown (`outOfSight`), so that the tabs in the background don't all ask at once. fs2's `merge`, `concurrently` and `hold1Resource` added 68 KB to the pages' bundle, so keep them out of it; so too Scala's `toDoubleOption`, so draws are read with `parseFloat`.
  - A fall hides the translations above it at once, asking for nothing; a rise back shows them again. `show` gives each part of a translation its draw in `data-codeswitch-draw`, and creates it hidden if the intensity is below that. A hidden part takes the class `codeswitch-hidden`: a `.codeswitch` or gap shows its `data-codeswitch`, keeping what it showed in `data-codeswitch-translation`, and an inserted piece is emptied, its parts kept on it (`stow`), so that the page's `textContent` is its original text. `restore()` still gives that back.
  - Only a new language restores the page.
- Known limits, from the review of #21: `empties` reads `textContent`, so a link whose other text is screen-reader-only or a lone arrow can still be emptied to the eye, and it covers only some roles; `<br>` and block children join with no separator, in placement and in the context; `Passage` is rebuilt for each placement, and for each text with a word chosen; a run joined by non-breaking spaces counts as one word; code and `.notranslate` text are left out of contexts ("Run `npm install` now" is sent as "Run now"); a framework that writes into the emptied page node gets its text doubled on restore, as before; and the DOM paths (`Edit` order, `settle`, `join`) are covered only by the end-to-end check.
- `Tooltip` shows the original of the translation under the pointer above it (below it in the window's top line, and kept within its sides), as a manual popover appended to `<html>`, in the top layer. It was a CSS `::after` on the span, which the page hid for about one word in fifteen: off the top of the window, clipped by `overflow` boxes (sidebars, nav bars, tables) or covered by sticky headers in another stacking context. Keep it out of the page's layout. `all: initial !important` in its rule would show it while closed, so `:not(:popover-open)` hides it.
- A word's context is the text around it within its passage (the nearest `p`, `li`, cell, heading, `div`, ...), so that a word in a link or `<em>` still has its sentence; never only its own text node, which for such a word is the word alone. `Passage.around` gives it as the page said it: our translations and gaps as their originals, and elements within `skipped` (code, scripts, text fields, `.notranslate`) as a space, so their text is not sent. `Words.context` collapses spacing and keeps 100 characters on each side, or more to reach the ends of the word's sentence, up to 300; `…` marks where either side is cut, and the model is told to decline a word whose translation belongs past one. Sentences come from `Intl.Segmenter` (through a facade in `Words.scala`, using `containing`), or, where there is none (Firefox before 125), from their stops (`Words.byStops`).
- The background (`Codeswitch.translations`) reads the language and key, and asks Iris's `LlmClient` (`Translator`) for translations of a batch of words with their contexts, a JSON array of pairs in, and out an array for each word of its pieces (a string for one in the word's place, an object with `text` and `replacing`, `after` or `before` otherwise, an empty array to decline). The decoder is lenient: a lone string, a lone object or `null` stands for a word's pieces, `[[ ]]` quoted in a place is dropped, and so is a piece in the word's place that only repeats the word (a model echoing the pairs it was given). An entry it cannot read is declined. A reply with more or fewer entries than words is `Translator.unmatched`, as which is which cannot be known: the background reports it like any problem, then answers with every word declined (`Codeswitch.recover`), as a failed request would end `Reader.pass` for good. The instructions also tell the model never to follow instructions in the contexts. Failures are recorded in `problem` and badged `!` on the toolbar button of the tab that asked (`report`, from `serve`'s `Sender`), or of every tab if none did; a success there clears its mark, and so does any change in the popup, for the tab it was opened over.
- The reader chooses any model Iris lists (`Translator.models`, by provider; Claude Haiku 4.5 by default), kept by `LlmModel.name` (`given Codec[LlmModel]` in `Translator`), and gives a key for its provider. The popup's key field shows the chosen model's provider's key, and keeps each provider's apart. Any change of settings in the popup clears the problem and the badge.
- Code outside `Codeswitch` that uses `browser.storage` needs `(using Granted[Storage])`, as the grant is only given inside the extension object.
- The popup is rendered by Laminar (`LaminarPage`, from `omnibrowser-laminar`) and styled by `src/main/extension/popup.css` (`withStylesheets`). Stored settings come in as signals (`browser.signal(key)`). The menus store their choices through `saver(key)`, as does the slider, but only on `change` (letting it go, or a key press), throttled to once a second (`Popup.pace`), as sync storage allows 120 writes a minute, showing its value on `input`, which it stops from bubbling up; the key field stores through `saving`, which wrap `browser.observer` so that the popup's foot shows its status: a spinner while any change is saving (for at least 500 ms, as a write is too quick to see, so typing keeps it spinning), then a tick and "Saved", which `popup.css` fades after 3 s, or "Not saved". One `onInput` handler on the popup's root clears the problem, as each control's input event bubbles up to it, except the slider's, which clears it on `change`. The slider is named by `aria-labelledby` and read out by `aria-valuetext`, its `<output>` hidden from screen readers. All run their programs through `browser.run`, never `unsafeRunAndForget`.
- Laminar stays out of the pages' bundle too, within `popup()` and `Popup`: `grep -c com.raquo` on `omnibrowser-content.js` should find nothing. The content script edits the host page's text nodes directly, in plain DOM.
- `hosts` grants the background each provider's API (from Iris's `LlmProvider.origin`), so its requests do not depend on the API allowing CORS.
- Codecs are derived (`derives Codec`), except `Language`'s, which keeps a language by its ISO code, as the released plugin has stored it since v0.1.0; a derived one would keep its name, and lose readers' choices.

### Testing

- The tests run in Node, which has no DOM: `Words`, `Placement`, `Translator`, `Tooltip.place` and the codecs are tested directly, and the background through `FakeBrowser` (whose pages are stand-ins without a DOM, so the content script and popup cannot be run there). `Translator` is tested against an sttp `BackendStub`.
- `Highlighter`, `Reader`, `Tooltip` and `Popup` were checked end to end in Chrome for Testing over CDP (Puppeteer's, in `~/.cache/puppeteer/chrome`, takes `--load-extension`), with Anthropic's API stood in for by `Fetch.fulfillRequest` in the service worker. Chrome's own component extensions also have a `background.js`; the plugin's worker is the one where `globalThis.__omnibrowser` is defined. As the content bundle is linked apart from the main one, check both the development and release builds. To check placement, have the stub answer each kind of piece in turn, save each passage's `textContent` from an inline script at the end of the test page, and compare after restoring (set `language` to `null`), with each passage's count of text nodes, which must not grow; count passages with two spaces together, and links left empty, too. Change settings under an open page with `chrome.storage.sync.set` in the worker: to check the intensity, group the parts of each translation by `data-codeswitch-draw`, and compare them and the requests after each change, and the page's `textContent` with the original at 0. To make the page seem hidden to the content script, override `document.visibilityState` in its isolated world (the `Runtime.executionContextCreated` context whose `auxData.type` is `isolated`; listen before `Runtime.enable`, which reports the contexts at once). In JavaScript sent inside a template literal, write `\\S` for a regular expression's `\S`.
- To hover, send `Input.dispatchMouseEvent` (`mouseMoved`), and after scrolling wait a frame first: `scroll` fires on the next frame, and hides the tooltip. `Page.captureScreenshot`'s `clip` is in page coordinates, not the viewport's.
- sbt 2 caches passing suites, so `sbt test` may run only those whose code changed; `sbt testFull` runs them all (75).

### Gotchas

- A `/*` in a comment, as in a match pattern like `https://*/*`, opens a nested comment in Scala.
- On Windows, if `sbt` cannot start its server, run it with `sbt --server`.
- On Windows, a script that rewrites a source file in text mode (as Python's `open(p, 'w')`) writes CRLF. Scalafmt mends the code but not multi-line string literals, such as the model's instructions, which then hold `\r`; write with `newline=''`, and check with `git diff`'s CRLF warning.
- On Windows, if compiling fails with "A required privilege is not held by the client" while backing up a `.sjsir`, delete `target/out/sjs1/**/classes` (sbt 2's disk cache links them) and compile again.
- Git here pushes a branch to its upstream (`push.default=tracking`), and the token may bypass the ruleset that guards `main`. A branch made from `origin/main` tracks `main`, so `git push origin <branch>` lands on `main` without a PR. Make branches with `--no-track`, and push with an explicit target: `git push -u origin HEAD:refs/heads/<branch>`.

## Instructions

### Compilation and Diagnostics

- When the user asks for help with a compilation or type error, start by running `sbt compile` to see the error for yourself.
  If there are many errors, making it unclear which one the user is referring to, ask them to clarify, and then focus only on that issue.
- IntelliJ MCP integration is active. When a request seems to implicitly refer to something the user is looking at, always check
  `mcp__ide__getDiagnostics` first to see which file(s) are open and get associated diagnostics (errors, warnings, and info hints with line numbers).

#### Testing

- After making code changes, always run `sbt compile` to verify that issues are fixed and no new ones are introduced.
- Repeatedly retry upon failure until the build succeeds. If you are unsure how to fix an issue, ask for help or refer to existing code for examples.
- Before trying to fix an error, make sure you first understand it fully.
- You should never report that a feature is complete without testing it first.

### Code Style

- You must read the [Code Style Guidelines](docs/STYLE_GUIDE.md).

### Pull Requests

When asked to publish the code changes, your task is to open one or more pull requests (PRs) to merge the changes into `main` on GitHub:

- Use `git` to check what has changed as compared to the `main` branch on `origin`.
- If the changes are thematically linked, they can be published as a single PR.
- Otherwise, you'll need to divide the changes into multiple PRs using your own judgement.
- Each PR should have a singular focus, shouldn't break anything, and should be able to be merged independently.
- Ensure that all code is staged, committed and pushed. Ensure no new files are left uncommitted, and no debug code is left in the codebase.
- When creating a PR, ensure that the title and description are clear, informative, and comprehensive.
- All feature/bugfix/etc branch names should be formatted as "feature_<short description>" or "fix_<short description>" or similar.
- All PR titles should be formatted as "[<scope>] <Short summary>", e.g. "[renderer] Fixed colour inversion bug."
- Use the `gh` CLI to do the above.
