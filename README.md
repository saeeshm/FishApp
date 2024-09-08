
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FishApp

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This is a shiny dashboard that calculates a suite of fishery-dependent empirical indicators for assessing the state of artisanal fisheries on the Northern Coast of Honduras. The application currently utilizes data gathered from the artisanal fisheries survey conducted in Tela Bay, but is suited to any database structured in the same format (including the ongoing survey databases for Trujillo and Islas de Bahía).

## Installation

There are two ways to install the development version of FishApp. For both, you will first need to open Rstudio or an R Console. Next, you will need to install the R packages `remotes` and `devtools`. This can be done by running the following commands in the terminal:

```
# MUST BE DONE INSIDE RSTUDIO OR AN R CONSOLE
install.packages('remotes')
install.packages('devtools')
```

### Direct installation from github
```
remotes::install_github('https://github.com/saeeshm/FishApp', force=T)
```

### Installation from a zip file
You will first need to download this repository as a zip file or have someone send you a zip version of the package.
```
# REPLACE THE PATH INSIDE THE QUOTES WITH THE FILE PATH TO YOUR FOLDER
remotes::install_local('/path/to/FishApp-master.zip', force=T)
```

## Updating the application
When there are changes made to the application that you would like to see, you will need to re-install the packages in it's entirety. Do so, simply run the above installation commands again using whichever method you prefer. 

Make sure to always use the MASTER branch when installing, as this is the most tested and up-to-date version of the app.

## Running the dashboard
Once installed, the dashboard can be run from within Rstudio as follows:
```
library(FishApp)
run_app()
```