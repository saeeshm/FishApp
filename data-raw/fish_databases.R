# Author: Saeesh Mangwani
# Date: 2026-10-12

# Description: Builds the `databases` object: a named list holding the cleaned
# catch (fdb), length-frequency (lfqdb) and species-name (spectab) tables for
# each region/habitat, plus the shared trophic-level crosswalk. Run from the
# project root.
#
# Inputs:
#   data-raw/stock_analysis_data/taxa_trophic_level_cwalk.csv
#   data-raw/stock_analysis_data/{region}_{habitat}_sa_raw.csv
#   data-raw/stock_analysis_data/{region}_{habitat}_lfq_adjust.csv
#
# Outputs: data/databases.rda

# ==== Packages ====
library(data.table)

# ==== Trophic level data (shared by all databases) ====
troph_cwalk <- fread('data-raw/stock_analysis_data/taxa_trophic_level_cwalk.csv')
troph_cwalk[, species := ifelse(is.na(species) & !is.na(genus), 'sp', species)]
troph_cwalk[, nombre_cientifico := paste(genus, species)]
troph_cwalk[, troph := ifelse(is.na(troph_diet), troph_food, troph_diet)]

# ==== Helper function ====

# Prepares a single region/habitat database for inclusion in app
prep_fish_dbases <- function(fdb_path, lfqdb_path, troph_cwalk){
  # Reading in the cleaned databases
  fdb <- fread(fdb_path)
  lfqdb <- fread(lfqdb_path)

  # Renaming gear-types
  lfqdb$tipo_arte <- dplyr::case_when(
    lfqdb$tipo_arte == 'Gillnet - 2in' ~ 'Trasmallo - 2"',
    lfqdb$tipo_arte == 'Gillnet - 3in' ~ 'Trasmallo - 3"',
    lfqdb$tipo_arte == 'Gillnet - Unknown' ~ 'Trasmallo - Desconocido',
    lfqdb$tipo_arte == 'Handline' ~ 'Linea de Mano',
    T ~ lfqdb$tipo_arte
  )

  # Joining trophic level data to lfqadj database
  setkey(lfqdb, nombre_cientifico)
  setkey(troph_cwalk, nombre_cientifico)
  lfqdb <- merge(
    lfqdb,
    troph_cwalk[, c('nombre_cientifico', 'troph'), with=F],
    by='nombre_cientifico'
  )

  # Species/scientific name selection table
  fdb$nombre_comun_cln <- fdb$nombre_comun
  spectab <- unique(na.omit(fdb[,.(nombre_comun_cln, nombre_cientifico)]))
  # Filtering only those also present in the lfq-adjusted table
  spectab <- spectab[nombre_cientifico %chin% unique(lfqdb$nombre_cientifico),]

  # Some manual re-assignments (We assume these are universal for all regions,
  # this may change, review this assumption with local experts):

  # Tela and Trujillo:
  # Always using "grupa" for groupers (sometimes labelled as "mero"):
  spectab[,nombre_comun_cln := ifelse(nombre_cientifico == 'Epinephelus sp', 'grupa', nombre_comun_cln)]
  # Setting Scombemorus cavalla to "macarela" based on Antonella's guidance
  spectab[,nombre_comun_cln := ifelse(nombre_cientifico == 'Scomberomorus cavalla', 'macarela', nombre_comun_cln)]
  # Setting Luntjanus sp to only "chinita", as both other classifications have assigned species names
  spectab[,nombre_comun_cln := ifelse(nombre_cientifico == 'Lutjanus sp', 'chinita', nombre_comun_cln)]
  # Removing duplication for Callinectes sp, which when ambiguous should assume the Blue Crab jaiba azul
  spectab[,nombre_comun_cln := ifelse(nombre_cientifico == 'Callinectes sp', 'jaiba azul', nombre_comun_cln)]
  # Removing duplication for Centropomus sp, which when ambiguous should assume
  # the common name "robalo"
  spectab[,nombre_comun_cln := ifelse(nombre_cientifico == 'Centropomus sp', 'robalo', nombre_comun_cln)]
  # Removing duplication for Oreochromis sp, which when ambiguous should assume the common name "tilapia"
  spectab[,nombre_comun_cln := ifelse(nombre_cientifico == 'Oreochromis sp', 'tilapia', nombre_comun_cln)]

  # Trujillo:
  # Removing duplication for Eugerres plumieri, which when ambiguous should assume the common name "caguacha"
  spectab[,nombre_comun_cln := ifelse(nombre_cientifico == 'Eugerres plumieri', 'caguacha', nombre_comun_cln)]
  # Removing duplication for Centropomus undecimalis, which when ambiguous should assume the common name "robalo"
  spectab[,nombre_comun_cln := ifelse(nombre_cientifico == 'Centropomus undecimalis', 'robalo', nombre_comun_cln)]
  # Removing duplication for Caranx hippos, which when ambiguous should assume the common name "jurel cola amarilla"
  spectab[,nombre_comun_cln := ifelse(nombre_cientifico == 'Caranx hippos', 'jurel cola amarilla', nombre_comun_cln)]
  # Removing duplication for Calamus pennatula, which when ambiguous should assume the common name "jurel cola amarilla"
  spectab[,nombre_comun_cln := ifelse(nombre_cientifico == 'Calamus pennatula', 'pez pluma', nombre_comun_cln)]
  # Removing duplication for Lutjanus synagris, which when ambiguous should assume the common name "calale"
  spectab[,nombre_comun_cln := ifelse(nombre_cientifico == 'Lutjanus synagris', 'calale', nombre_comun_cln)]
  # Deduplicating table following changes:
  spectab <- setkey(unique(spectab), nombre_cientifico)

  # Joining cleaned common names back to the LFQ-adjusted table
  lfqdb <- merge(
    lfqdb,
    spectab[, c('nombre_cientifico', 'nombre_comun_cln'), with=F],
    by='nombre_cientifico'
  )

  return(list(
    fdb = fdb,
    lfqdb = lfqdb,
    spectab = spectab
  ))
}

# ==== Tela data ====

# Tela Marine data objects
tela_marino <- prep_fish_dbases(
  fdb_path = 'data-raw/stock_analysis_data/tela_marino_sa_raw.csv',
  lfqdb_path = 'data-raw/stock_analysis_data/tela_marino_lfq_adjust.csv',
  troph_cwalk = troph_cwalk
)
names(tela_marino) <- paste0('tela_marino_', names(tela_marino))
# Tela lagoon data objects
tela_laguna <- prep_fish_dbases(
  fdb_path = 'data-raw/stock_analysis_data/tela_laguna_sa_raw.csv',
  lfqdb_path = 'data-raw/stock_analysis_data/tela_laguna_lfq_adjust.csv',
  troph_cwalk = troph_cwalk
)
names(tela_laguna) <- paste0('tela_laguna_', names(tela_laguna))

# ==== Trujillo data ====

# Trujillo marine data objects
truj_marino <- prep_fish_dbases(
  fdb_path = 'data-raw/stock_analysis_data/trujillo_marino_sa_raw.csv',
  lfqdb_path = 'data-raw/stock_analysis_data/trujillo_marino_lfq_adjust.csv',
  troph_cwalk = troph_cwalk
)
names(truj_marino) <- paste0('truj_marino_', names(truj_marino))
# Trujillo lagoon data objects
truj_laguna <- prep_fish_dbases(
  fdb_path = 'data-raw/stock_analysis_data/trujillo_laguna_sa_raw.csv',
  lfqdb_path = 'data-raw/stock_analysis_data/trujillo_laguna_lfq_adjust.csv',
  troph_cwalk = troph_cwalk
)
names(truj_laguna) <- paste0('truj_laguna_', names(truj_laguna))

# ==== Saving ====
databases <- c(list('troph_cwalk' = troph_cwalk), tela_marino, tela_laguna, truj_marino, truj_laguna)
usethis::use_data(databases, overwrite = TRUE)
