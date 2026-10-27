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
publishes with [`posit-dev/connect-actions`](https://github.com/posit-dev/connect-actions), the
supported successor to the deprecated `rstudio/actions/connect-publish`.

The `deploy` action **updates an existing content item**; it does not create net-new content.
Deploy this app to Connect once by any means (for example the Positron/VS Code Publisher extension or
`rsconnect::deployApp()`) to obtain its content GUID, then wire up the repository.

Add the following under **Settings > Secrets and variables > Actions**:

- **`CONNECT_SERVER`** (secret) — your Connect base URL (e.g. `https://connect.example.com`).
- **`CONNECT_API_KEY`** (secret) — a Connect API key owned by a publisher account. It is read only
  from the repository secret and is never committed.
- **`CONNECT_CONTENT_GUID`** (variable) — the GUID of the existing content item to update.

Package dependencies are managed with [`renv`](https://rstudio.github.io/renv/):
`r-lib/actions/setup-renv` restores the pinned `renv.lock`, the workflow regenerates `manifest.json`
with `rsconnect::writeManifest()`, and the `deploy` action publishes that manifest to Connect with
`posit connect deploy manifest`.

> **Trusted publishing:** `posit-dev/connect-actions` also supports keyless
> [**trusted publishing**](https://docs.posit.co/connect/user/trusted-publishing/) (OIDC), which
> removes the need for a `CONNECT_API_KEY`. It requires Connect 2026.07.0+ with an Enhanced or
> Advanced license. To use it, authorize this repository on the content's **Source** settings, drop
> `connect-api-key` from the workflow, and add `id-token: write` to the job's `permissions`.
