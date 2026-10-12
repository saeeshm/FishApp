# FishApp

<!-- badges: start -->
[![Live app](https://img.shields.io/badge/live%20app-shinyapps.io-blue.svg)](https://coralreefalliance.shinyapps.io/fishapp/)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

This is the working repository for ongoing maintenance of the artisanal fisheries assessment tool, a Shiny app that calculates a suite of fishery-dependent empirical indicators for assessing the state of artisanal fisheries. This work was developed in partnership with the [Coral Reef Alliance](https://coral.org/), focusing on their ongoing fishery monitoring efforts on the Northern Coast of Honduras.

**The tool is live at <https://coralreefalliance.shinyapps.io/fishapp/>.**

The app currently maintains data from the artisanal fisheries surveys conducted in Tela Bay and Trujillo, and is suited to fitting stock indicators for any survey database structured in the same format (including the ongoing survey database from Las Islas de Bahía).

## Using the tool

If you are interested in using the tool or exploring its capabilities, please use the live app linked above. No installation is required. The dashboard is available in Spanish and English, and is organized into the following pages:

- **Description:** an overview of the tool and its history
- **Map:** the monitored fishing communities and management areas for each region
- **General Summaries:** sampling effort, catch composition (common species, gear types), seasonality, and a timeline of key management milestones
- **Indicators by Species:** indicators for individual species, including CPUE, CPUE-per-length class, catch volumes, and the proportion of mature, immature, and mega-spawner individuals in the catch
- **Multispecies Indicators:** indicators across the whole catch, including CPUE, species diversity (richness, Shannon diversity, evenness), length-frequency, and a biomass density index.

## Contact

If you are interested in the technical details of the indicators, or would like your own fisheries data to be included in the app, please contact the app author, Saeesh Mangwani (saeesh.mangwani@imbrsea.eu), for support.

## Notes for developers

FishApp is built as an R package using the [golem](https://thinkr-open.github.io/golem/) framework. If you are interested in contributing to its development, please get in touch with the app author (see [Contact](#contact)) to get started. The steps below describe how to set up a working copy.

### 1. Clone the repository

Clone this repository and open `FishApp.Rproj` in RStudio. Package versions are pinned with [renv](https://rstudio.github.io/renv/), so restore the project library before doing anything else:

```
renv::restore()
```

### 2. Get the data

The fisheries survey data this app runs on is sensitive, so it is **not included in this repository**. The following are excluded from version control:

- `data/*.rda`: the built datasets the app loads at runtime
- `data-raw/stock_analysis_data/`: the raw catch and length-frequency databases
- `data-raw/spatial_data/`: community locations and management-zone map layers

Without these files, the package will load but the app will not run. Everything else is public: the app code, the dataset documentation in `R/data.R` (e.g. `?databases`), the scripts in `data-raw/` that build each dataset, and the app text in `data-raw/app_metadata/`.

Please contact the app author to request access to the data. Once you have the source files, place them in the folders listed above and rebuild the datasets by running the following from the project root:

```
for (f in list.files("data-raw", pattern = "\\.R$", full.names = TRUE)) source(f)
```

### 3. Set up the map API key

The map page uses [Carto](https://carto.com/) basemap tiles, which require an API key. Keys are read from environment variables and are never stored in the repository. For local development, create a `.Renviron` file in the project root (it is git-ignored) containing:

```
CARTO_API_KEY_LOCAL=<your-key>
```

Without a key the app still runs, but the basemap tiles on the map page will not load. The app author can provide a development key if required.

### 4. Run the app

Load the package and launch the app in development mode:

```
devtools::load_all()
run_app()
```

### Project structure

- `R/`: app code. Each dashboard page is a golem module (`mod_01_` to `mod_06_`), with its helper functions in a matching `_utils.R` file. Dataset documentation lives in `R/data.R`.
- `data-raw/`: one script per dataset, plus their source files (see [Get the data](#2-get-the-data)).
- `data/`: built datasets, lazy-loaded by the package.
- `inst/app/www/`: static assets (logos, `brand.yml` theme, custom CSS).
- `dev/`: golem development scripts.

### Deployment

The live app is deployed to shinyapps.io by the app author. Deployment credentials and the production map key are kept outside the repository. Please coordinate with the app author before attempting to deploy any changes.

## License

MIT © Saeesh Mangwani. See [LICENSE.md](LICENSE.md).

## Acknowledgements

This tool was developed in partnership with the [Coral Reef Alliance](https://coral.org/), and was supported by grants from [CORAL](https://coral.org/)) as well as the [IMBRSea Program at the University of Ghent](https://www.imbrsea.eu/). It would not have been possible without the support and guidance of Dr. Antonella Rivera and Dr. Mayra Núñez-Vallecillo.
