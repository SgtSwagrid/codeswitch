<div align="center">
  <h1>🔌 Scala Browser Plugin Template</h1>
  <p>A reusable template for browser plugins in Scala.</p>
</div>

<br><br>

> "Simplicity is prerequisite for reliability." — Edsger W. Dijkstra.

<br>

## 📋 What's included?

1. Everything from [Scala Browser Plugin Config](https://github.com/SgtSwagrid/scala-browser-plugin-config), including reasonable [Scalafmt](https://scalameta.org/scalafmt/) settings, CI pipelines for build integrity, and some IDE config.
2. [Omnibrowser](https://github.com/SgtSwagrid/omnibrowser), with which the plugin is written in Scala against a typed model of the browser, and which builds it for Chrome and Firefox.
3. Automatic packaging of each release for every browser, ready for its store.
4. An example plugin with a test, example build configuration, and setup instructions.

## 🔨 How to use this template

### 1. Create your repository

Click '[**Use this template**](https://github.com/new?template_name=scala-browser-plugin-template&template_owner=SgtSwagrid)' on GitHub, and follow the instructions to create a new repository for your plugin.
All files herein will be copied as-is.

### 2. Configure [build.sbt](build.sbt)

Replace every placeholder with real values for your project:

| Name | Purpose | Example |
| ---- | ------- | ------- |
| `organization` | Your organisation's package namespace, which also names your plugin in Firefox (as `<project>@<organization>`). | `org.nohungrydogs` |
| `packagePrefix` | IntelliJ's implicit package prefix for all code files. | `org.nohungrydogs.dogfoodfinder` |
| Name of the project (following `lazy val`) | Your plugin's name, which also names its packages. | `dog-food-finder` |
| Scaladoc title (`-project` in `ScalaUnidoc / unidoc / scalacOptions`) | The title of your plugin's Scaladoc site. | `Dog Food Finder` |

### 3. Write your plugin

Replace [Example.scala](src/main/scala/Example.scala) with your own plugin: an object extending `BrowserExtension`, whose type names the permissions it requests.
Its `name` and `description` are those the browser shows.
See [Omnibrowser](https://github.com/SgtSwagrid/omnibrowser) for the model of the browser that it is written against.

Files placed in `src/main/extension`, such as your plugin's icons, are copied into the plugin as they are.

### 4. Add repository secrets

Add the following secrets to your repository on GitHub, to allow the automated workflows to run:

| Secret                    | Value                                                                                                                                                               |
|---------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `GH_TOKEN`                | A GitHub [PAT](https://github.com/settings/personal-access-tokens) with read and write access to the contents, pull requests and administration of your repository. |
| `CLAUDE_CODE_OAUTH_TOKEN` | OAuth token from `claude setup-token`, for [Claude Code](https://claude.com/product/claude-code) agentic workflows (optional).                                      |

Secrets can be added from the GitHub web interface by navigating as follows from your repository's page:

> **Settings → Secrets and variables → Actions**

### 5. Release your plugin

To release version `1.2.3`, go to **Releases → Draft a new release**, create the tag `v1.2.3`, and click **Publish release**.
The plugin is then packaged for each browser, and the zips are attached to the release, ready to be uploaded to the [Chrome Web Store](https://chrome.google.com/webstore/devconsole) and [Firefox Add-ons](https://addons.mozilla.org/developers/).

## 👮‍♂️ License

The included MIT license should be considered only as part of the template, and is not binding.
This repository is hereby released to the public domain, to be used freely.
In particular, and contra [LICENSE.md](LICENSE.md), you may remove the license text from copies.

## 🤝 Contributing

[CONTRIBUTING.md](CONTRIBUTING.md) is also part of the template, and does not _necessarily_ apply to contributions to the template itself.
The most important thing to know is that many of the configuration files are automatically synced from [Scala Browser Plugin Config](https://github.com/SgtSwagrid/scala-browser-plugin-config), and should be updated there rather than here.

## 👁️ See also

- Check out [Scala Library Template](https://github.com/SgtSwagrid/scala-library-template) and [Scala Website Template](https://github.com/SgtSwagrid/scala-website-template) for similar templates to quickly start a new library or full stack website in Scala.
- This project is configured by [Scala Browser Plugin Config](https://github.com/SgtSwagrid/scala-browser-plugin-config).

<br/><br/><br/><br/>
<h3 align="center">⬆️ Delete • Keep ⬇️</h3>
<br/><br/><br/><br/>

<div align="center">

  <h1>✨ My Plugin</h1>
  <p>A very cool browser plugin that does something great.</p>

  <!-- Update the following URL to show live build status in your README. -->
  <span>
    <a href="https://github.com/SgtSwagrid/scala-browser-plugin-template/actions/workflows/build-integrity.yml"><img src="https://github.com/SgtSwagrid/scala-browser-plugin-template/actions/workflows/build-integrity.yml/badge.svg" alt="Build status" /></a>
  </span>

</div>

## ⬇️ Installation

<!-- Replace with the address of your own repository. -->
Download the zip for your browser from the [latest release](https://github.com/SgtSwagrid/scala-browser-plugin-template/releases/latest), then:

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
