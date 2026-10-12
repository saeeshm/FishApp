# Author: Saeesh Mangwani
# Date: 2026-10-12

# Description: Documentation for the package datasets in data/. Each dataset is
# built by the script of the same name in data-raw/.

# ==== Fish databases ====

#' Fish monitoring databases
#'
#' Cleaned catch and length-frequency databases for each monitored
#' region/habitat, plus the trophic-level crosswalk shared between them.
#'
#' @format A named list of `data.table`s:
#' \describe{
#'   \item{troph_cwalk}{Trophic-level crosswalk by `nombre_cientifico`, with
#'     the chosen trophic level in `troph` (diet-based where available,
#'     otherwise food-item-based).}
#'   \item{\{region\}_\{habitat\}_fdb}{Raw catch-landings (stock analysis)
#'     database, with a cleaned common name column `nombre_comun_cln`.}
#'   \item{\{region\}_\{habitat\}_lfqdb}{Length-frequency adjusted database,
#'     with gear types translated to Spanish (`tipo_arte`) and joined to
#'     `troph` and `nombre_comun_cln`.}
#'   \item{\{region\}_\{habitat\}_spectab}{Lookup of `nombre_comun_cln` to
#'     `nombre_cientifico` for species present in the LFQ database.}
#' }
#' where `region` is one of `tela` or `truj` and `habitat` is one of `marino`
#' or `laguna`.
#' @source data-raw/fish_databases.R, from data-raw/stock_analysis_data/.
"databases"

# ==== Map data ====

#' Tela fishing communities
#'
#' @format An `sf` point layer of fishing community locations in Tela.
#' @source data-raw/datos_mapa.R, from data-raw/spatial_data/.
"tela_locs"

#' Trujillo fishing communities
#'
#' @format An `sf` point layer of fishing community locations in Trujillo.
#' @source data-raw/datos_mapa.R, from data-raw/spatial_data/.
"truj_locs"

#' Tela fish recovery zones
#'
#' Zonas de recuperacion pesquera (ZRPs) of Tela Bay.
#'
#' @format An `sf` polygon layer, with the zone name in `Nombre`.
#' @source data-raw/datos_mapa.R, from data-raw/spatial_data/.
"zdr"

#' Honduran marine-coastal protected areas
#'
#' Areas protegidas marino-costeras (APMC) of Honduras, as of 2019.
#'
#' @format An `sf` polygon layer, including `NOMBRE` (area name), `CATEGORIA`
#'   (protection category) and `lab` (a list of HTML labels for map popups).
#' @source data-raw/datos_mapa.R, from data-raw/spatial_data/.
"apmc"

# ==== App text ====

#' Plot and card descriptions
#'
#' Bilingual titles and Markdown descriptions shown in the dashboard modals,
#' looked up by `get_desc()`.
#'
#' @format A `data.table` with columns:
#' \describe{
#'   \item{idx}{Description key, e.g. `num_muestreos`.}
#'   \item{category}{Page the description belongs to, e.g. `Datos Generales`.}
#'   \item{title_en, title_es}{Title in English and Spanish.}
#'   \item{desc_en, desc_es}{Markdown description in English and Spanish.}
#' }
#' @source data-raw/card_descriptions.R, from
#'   data-raw/app_metadata/card_descriptions.yaml.
"card_descriptions"

#' Management milestone timeline
#'
#' Key management milestones per database, shown by `dg_timeline()`.
#'
#' @format A `data.table` with columns:
#' \describe{
#'   \item{database}{Database name, e.g. `Tela` or `Trujillo`.}
#'   \item{sort_order}{Left-to-right order of the milestone.}
#'   \item{period_en, period_es}{Period label (year or month) in English and
#'     Spanish. Blank means the milestone continues the previous period.}
#'   \item{label_en, label_es}{Milestone caption in English and Spanish.}
#' }
#' @source data-raw/timeline_milestones.R, from
#'   data-raw/app_metadata/timeline_milestones.csv.
"timeline_milestones"

#' Gear name lookup
#'
#' Lookup table translating the standardized (Spanish) gear names in
#' `databases` to display names in English and Spanish, with a consistent
#' display order and colour per gear. Used by `translate_gear()`.
#'
#' @format A `data.table` with columns:
#' \describe{
#'   \item{tipo_arte}{Standardized gear name, as it appears in the `tipo_arte`
#'     column of `databases`. This is the join key.}
#'   \item{sort_order}{Display order of the gear in plots and legends.}
#'   \item{name_es, name_en}{Gear display name in Spanish and English.}
#'   \item{colour}{Hex colour for the gear, from the CORAL palette.}
#' }
#' @source data-raw/gear_names.R, from data-raw/app_metadata/gear_names.csv.
"gear_names"
