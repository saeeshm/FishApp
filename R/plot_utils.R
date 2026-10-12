# Author: Saeesh Mangwani
# Date: 2024-02-19

# Description: Shared utility functions for the entire app. Functions defined
# broadly deal with plot themeing, font management, card design, and querying
# databases for appropriate data based on provided filters

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


# Themeing and plot functions ----------

# Adding google fonts
sysfonts::font_add_google('Inter', 'Inter')
showtext::showtext_auto()

# Generic ggplot theme
qtheme <- function(base_size=24, base_family='Inter'){
  ggplot2::theme_minimal(base_size, base_family) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill='white', colour=NA),
      # legend.position = 'bottom',
      panel.border = ggplot2::element_rect(colour = "black", fill=NA, linewidth=0.5),
      strip.text = element_text(size=rel(0.8), face='bold'),
      legend.title = element_text(size=rel(0.8)),
      axis.title = element_text(size=rel(0.8)),
      axis.text = element_text(size=rel(0.6)),
      legend.text = element_text(size=rel(0.6)),
    )
}

# Coral colour palette (used for styling plots with manual scales)
cols_coral = c(
  "#3ebbed","#f69a94","#eddfca","#90d6e5","#00afa4","#0d659a","#a8426e"
)

# Helper function to render a plot card with some description text at the bottom
# and optionally at the top. It is assumed that the labels are provided as html
# parseable taglists
plot_card_labelled <- function(plot, bottom_label=NULL, card_title=NULL, top_label = NULL){
  # Option of no top label
  if(is.null(top_label)){
    currbody <- bslib::card_body(
      class = 'align-items-center',
      plot,
      tagList(bottom_label)
    )
  # Option of no bottom label
  }else if(is.null(bottom_label)){
    currbody <- bslib::card_body(
      class = 'align-items-center',
      tagList(top_label),
      plot
    )
  # Option of both labels
  }else{
    currbody <- bslib::card_body(
      class = 'align-items-center',
      tagList(top_label),
      plot,
      tagList(bottom_label)
    )
  }
  # If there is no header/title, just returning the card body
  if(is.null(card_title)){
    return(currbody)
  }
  # Otherwise returning a full card object
  bslib::card(
    fill = T,
    full_screen = TRUE,
    bslib::card_header(card_title),
    currbody
  )
}