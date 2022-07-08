# Using GitHub Actions to Test & Deploy to RStudio Connect

RStudio Connect hosts a variety of data artifacts with different development 
life cycles. Whenever you want to publish one of these data artifacts to RStudio 
Connect, there are three paths you can follow:

 - Push-button deployment process within the RStudio IDE
 - Git-backed deployment within RStudio Connect
 - Programmatic deployment

This repository is an example of the third deployment path using GitHub Actions as
a CI/CD pipeline to test and deploy a Shiny application to RStudio Connect.

The `.github/workflows/test-and-connect-publish.yaml` file allows for the specification
of r-version and whether or not to use the public RSPM to install packages.
`renv` is used to restore the correct package environment before running tests with
shinytest2, creating a manifest.json file and publishing the Shiny application to RStudio Connect.