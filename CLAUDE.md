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

- `Codeswitch` wires the parts: storage keys (`language` and `model` in sync storage; `apiKeys`, keyed by `LlmProvider.key`, and `problem` in local), the `translate` endpoint, the background that answers it, the content script and the popup.
- The content script (`Reader`) watches the chosen language and, for each choice in turn (`switchMap`), restores the page, then translates its body and every node added to it, a pass at a time. `Highlighter` does all DOM work: it chooses words in text nodes (marking each node seen in the current pass, as split-off halves must not be rolled again), and replaces each with a `span.codeswitch` holding the original in `data-codeswitch`, which `src/main/extension/content.css` (put in every page by the manifest, through `withCss`) shows above it on hover.
- A word's context is the text around it within its passage (the nearest `p`, `li`, cell, heading, `div`, ...), read with a DOM `Range`, so that a word in a link or `<em>` still has its sentence; never only its own text node, which for such a word is the word alone. `Words.context` collapses spacing and keeps up to 100 characters on each side.
- The background (`Codeswitch.translations`) reads the language and key, and asks Iris's `LlmClient` (`Translator`) for translations of a batch of words with their contexts, a JSON array of pairs in and of strings out. Failures are recorded in `problem` and badged `!` (`report`).
- The reader chooses any model Iris lists (`Translator.models`, by provider; Claude Haiku 4.5 by default), kept by `LlmModel.name` (`given Codec[LlmModel]` in `Translator`), and gives a key for its provider. The popup's `KeyField` shows the chosen model's provider's key, and keeps each provider's apart. Any change of settings in the popup clears the problem and the badge.
- Code outside `Codeswitch` that uses `browser.storage` needs `(using Granted[Storage])`, as the grant is only given inside the extension object.
- The popup is static HTML (`Popup.body`, through `withBody`) styled by `src/main/extension/popup.css` (`withStylesheets`), which `Popup.show` fills in by element ID. DOM handlers run their effects through `browser.run`, never `unsafeRunAndForget`.
- `hosts` grants the background each provider's API (from Iris's `LlmProvider.origin`), so its requests do not depend on the API allowing CORS.
- Codecs are derived (`derives Codec`), except `Language`'s, which keeps a language by its ISO code, as the released plugin has stored it since v0.1.0; a derived one would keep its name, and lose readers' choices.

### Testing

- The tests run in Node, which has no DOM: `Words`, `Translator` and the codecs are tested directly, and the background through `FakeBrowser` (whose pages are stand-ins without a DOM, so the content script and popup cannot be run there). `Translator` is tested against an sttp `BackendStub`.
- `Highlighter`, `Reader` and `Popup` were checked end to end in Chrome for Testing over CDP, with Anthropic's API stood in for by `Fetch.fulfillRequest` in the service worker. Chrome's own component extensions also have a `background.js`; the plugin's worker is the one where `globalThis.__omnibrowser` is defined.

### Gotchas

- A `/*` in a comment, as in a match pattern like `https://*/*`, opens a nested comment in Scala.
- On Windows, if `sbt` cannot start its server, run it with `sbt --server`.
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
- You have GitHub MCP integration that can be used to do the above.
