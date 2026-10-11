# Using GitHub Actions to test & deploy to Posit Connect

Posit Connect hosts a variety of data artifacts with different development life cycles. Whenever you
want to publish one of these artifacts to Connect, there are three paths you can follow:

 - Push-button deployment from the IDE (RStudio / Positron)
 - Git-backed deployment within Posit Connect
 - Programmatic deployment

This repository is an example of the third path, using **GitHub Actions** as a CI/CD pipeline to
test and deploy a Shiny application to Posit Connect. For the full walkthrough see the Connect
documentation: *Deploy to Connect from CI/CD pipelines*.

## Setup

The workflow ([`.github/workflows/test-and-connect-publish.yaml`](.github/workflows/test-and-connect-publish.yaml))
publishes with [`rstudio/actions/connect-publish`](https://github.com/rstudio/actions). Before the
first run, add two repository secrets (**Settings > Secrets and variables > Actions**):

- **`CONNECT_SERVER`** — your Connect base URL *with a trailing slash*
  (e.g. `https://connect.example.com/`).
- **`CONNECT_API_KEY`** — a Connect API key owned by a publisher account. It is read only from the
  repository secret and is never committed.

Package dependencies are managed with [`renv`](https://rstudio.github.io/renv/):
`r-lib/actions/setup-renv` restores the pinned `renv.lock`, the workflow regenerates `manifest.json`
with `rsconnect::writeManifest()`, and the bundle is published to Connect. The content item is
created on the first publish and updated on each subsequent run.

> **Note:** Posit also maintains [`posit-dev/connect-actions`](https://github.com/posit-dev/connect-actions),
> which supersede `rstudio/actions/connect-publish` and add keyless **trusted publishing**. This
> example uses `rstudio/actions/connect-publish` with an API key; see the how-to for the
> trusted-publishing path.
