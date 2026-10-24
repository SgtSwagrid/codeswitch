# CLAUDE.md

This file provides guidance to [Claude Code](https://claude.com/product/claude-code) when working with code in this repository.
It is not intended for human eyes.

### Maintenance

You (robot or human) have standing permission to update this file without asking.
Add important patterns, gotchas, or context that would help future sessions.
Keep it concise and actionable.

## Project overview

This is a Scala browser plugin that does ...

- It is written with [Omnibrowser](https://github.com/SgtSwagrid/omnibrowser): one object extending `BrowserExtension` defines the plugin, and the `sbt-omnibrowser` plugin (added by the synced `project/plugins-browser-plugin.sbt`) compiles it to JavaScript with Scala.js and lays it out for Chrome and Firefox.
- `sbt omnibrowserBuild` builds it unpacked, for development, under `target/**/omnibrowser/development/<browser>/`, which a browser can load as it is.
  `sbt omnibrowserPackage` zips it for release, under `target/**/omnibrowser/release/`.
- Files in `src/main/extension`, such as icons, are copied into the plugin as they are.
- Versions come from git tags (`v1.2.3` is `1.2.3`), by sbt-dynver. Publishing a GitHub release packages it and attaches the zips (`publish.yml`).

### Gotchas

- A `/*` in a comment, as in a match pattern like `https://*/*`, opens a nested comment in Scala.
- On Windows, if `sbt` cannot start its server, run it with `sbt --server`.

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
