# Author: Saeesh Mangwani
# Date: 2024-02-19

# Description: Shared data utility functions for the entire app. Functions
# defined here query the package databases for appropriate data based on
# provided filters, and get metadata for populating sidebar input selectors.

# ==== Functions ====

# Database querying functions ----------

# Querying the appropriate length-frequency database based on the chosen region
pick_lfq_database <- function(dbname){
  # Choosing database based on region
  if(!dbname %in% c('Tela', 'Trujillo')){
    stop("No valid database found with that name! Check database picker input options")
  }
  # Choosing the appropriate database based on the dbname input
  db <- switch(
    dbname,
    'Tela' = databases$tela_marino_lfqdb,
    'Trujillo' = databases$truj_marino_lfqdb
  )
  return(db)
}

pick_fish_database <- function(dbname){
  # Choosing database based on region
  if(!dbname %in% c('Tela', 'Trujillo')){
    stop("No valid database found with that name! Check database picker input options")
  }
  # Choosing the appropriate database based on the dbname input
  db <- switch(
    dbname,
    'Tela' = databases$tela_marino_fdb,
    'Trujillo' = databases$truj_marino_fdb
  )
  return(db)
}

pick_spectab <- function(dbname){
  # Choosing database based on region
  if(!dbname %in% c('Tela', 'Trujillo')){
    stop("No valid database found with that name! Check database picker input options")
  }
  # Choosing the appropriate database based on the dbname input
  db <- switch(
    dbname,
    'Tela' = databases$tela_marino_spectab,
    'Trujillo' = databases$truj_marino_spectab
  )
  return(db)
}

# Functions for getting relevant metadata for populating sidebar input selectors
# based on the chosen database ----------

# Get the full range of years of data available for this database
get_year_periods <- function(dbname){
  db <- pick_fish_database(dbname)
  year_period <- range(year(na.omit(db$fecha)))
  # Appending the end of the range by 1 year so data within the final year
  # gets included
  year_period[2] <- year_period[2] + 1
  # Completing the whole range, and returning as a character vector (required
  # for sliderTextInput)
  as.character(seq(year_period[1], year_period[2]))
}

# Get the common names for the top 15-species (by capture amount) in the
# database, to use for populating the species picker input
get_spec_cmn_names <- function(dbname){
  curr_spectab <- pick_spectab(dbname)
  curr_fdb <- pick_lfq_database(dbname)
  # Top-15 species caught (only ones with sufficient sample size for analysis)
  top_specs <- curr_fdb[,.N, by=c('nombre_comun_cln')] |> setkey(x=_, N) |> tail(15)
  # Finaly database of species names to use
  db <- merge(top_specs, curr_spectab, by='nombre_comun_cln', all.x = T, all.y=F) |> 
    setkey(x=_, N)
  # Arranging by capture amount (descending)
  spec_cmn_names <- rev(stringr::str_to_title(unique(db$nombre_comun_cln)))
  return(spec_cmn_names)
}

# Get the scientific names for the top 15-species (by capture amount) in the
# database, to use for populating the species picker input
get_spec_sci_names <- function(dbname){
  curr_spectab <- pick_spectab(dbname)
  curr_fdb <- pick_lfq_database(dbname)
  # Top-15 species caught (only ones with sufficient sample size for analysis)
  top_specs <- curr_fdb[,.N, by=c('nombre_comun_cln')] |> setkey(x=_, N) |> tail(15)
  # Finaly database of species names to use
  db <- merge(top_specs, curr_spectab, by='nombre_comun_cln', all.x = T, all.y=F) |> 
    setkey(x=_, N)
  # Arranging by capture amount (descending)
  spec_sci_names <- rev(stringr::str_to_title(unique(db$nombre_cientifico)))
  return(spec_sci_names)
}

# Gear name translation ----------

# Translates standardized (Spanish) gear names to display names in the chosen
# language, as a factor ordered by the gear_names lookup table. Only gears
# present in x are kept as levels. lang is a plain value ('ES' or 'EN'), not a
# reactive. NOTE: only apply this to a filtered copy of the data, never with :=
# directly on the `databases` package object, as data.table modifies by
# reference and the change would persist across sessions and language toggles.
translate_gear <- function(x, lang){
  # Matching each gear to its row in the lookup table
  idx <- match(x, gear_names$tipo_arte)
  # Warning if any gears are missing from the lookup table
  if(anyNA(idx)){
    warning("Gear names not found in gear_names lookup: ",
            paste(unique(x[is.na(idx)]), collapse = ', '))
  }
  # Display name column for the chosen language
  name_col <- if(lang == 'ES') 'name_es' else 'name_en'
  # Gears present in x, in lookup table order
  ord <- gear_names[order(gear_names$sort_order), ]
  ord <- ord[ord$tipo_arte %in% x, ]
  factor(gear_names[[name_col]][idx], levels = ord[[name_col]])
}

# Returns the gear colours from the gear_names lookup table as a vector named
# by gear display name in the chosen language, for use as
# ggplot2::scale_fill_manual(values = gear_colours(lang)). Being named, the
# colours match the levels produced by translate_gear() regardless of which
# gears are present, keeping each gear's colour consistent across plots and
# databases. lang is a plain value ('ES' or 'EN'), not a reactive.
gear_colours <- function(lang){
  name_col <- if(lang == 'ES') 'name_es' else 'name_en'
  setNames(gear_names$colour, gear_names[[name_col]])
}
