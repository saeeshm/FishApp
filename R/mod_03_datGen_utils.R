# Author: Saeesh Mangwani
# Date: 2024-08-14

# Description: Utility functions for the sampling information page (essentially
# all the functions to generate the required plot based on reactive inputs)

#' @import data.table
#' @import ggplot2

# ==== Functions ====

# A spanish version of the month.abb vector
month.abb.esp <- c(
  'Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun',
  'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic'
)

# Getting summary statistics for the current year period
dg_summ_stats <- function(dbase, year_min, year_max){
  # Filtering the data for the requested year range
  currdf <- dbase[(year(fecha) >= year_min) & (year(fecha) < year_max), ]
  # List of required summary stats
  list(
    # Total number of samples in this year period
    num_muestreos = length(unique(currdf$codigo)),
    # Total number of days sampled
    num_dias_muestreo = length(unique(currdf$fecha)),
    # Communities sampled
    num_comunidades = unique(currdf$comunidad) |> na.omit() |> length(),
    # Named list of communities sampled
    nombres_comunidades = unique(currdf$comunidad) |> na.omit() |> paste(collapse=', '),
    # Total weight sampled - assuming already in kg (done during dataprep)
    peso_muestreado = round(sum(currdf$peso, na.rm=T), 0),
    # Total number of species sampled
    num_especies = uniqueN(currdf$nombre_cientifico),
    # Total number of individuals sampled
    num_individuos = nrow(currdf)
  )
}

# Plotting total weight sampled for the 10 most commonly caught species (by
# weight) from the current data
dg_plot_esp_comunes <- function(dbase, year_min, year_max, lang){
  # Filtering the data for the requested year range
  currdf <- dbase[(year(fecha) >= year_min) & (year(fecha) < year_max), ] |> 
    # Removing species with NA scientific names from this table (we only want to
    # plot identified fish species in the top 10)
    _[!is.na(nombre_cientifico),]
  
  # Getting the 10 most commonly caught species
  plotdf <- currdf |> 
    _[, .(peso = sum(peso, na.rm=T)), by=.(nombre_cientifico)] |> 
    _[, .(peso = sum(peso)), by=.(nombre_cientifico)] |> 
    _[order(-peso)] |> 
    head(10)
  
  # What proportion of weight do these 10 species comprise, relative to the
  # total caught weight
  prop_capt <- round(
    (sum(plotdf$peso)/(sum(currdf$peso, na.rm=T))) * 100,
    2
  )
  
  # Creating a description text element based on the proportion captured:
  prop_capt_desc <- cpue_mean_desc <- p(
    strong(ifelse(lang() == 'ES','Proporción del peso total capturado: ','Proportion of total captured weight: ')),
    paste0(prop_capt, '%')
  )
  
  # Plotting total weight by species
  top10_plot <- plotdf |> 
    # Reversing order of rows to be in descending order of weight
    _[order(peso)] |>
    # Factorizing species name in the descending order of weight
    _[, nombre_cientifico := factor(nombre_cientifico, levels=nombre_cientifico)] |>
    # Plotting
    ggplot2::ggplot(ggplot2::aes(y = nombre_cientifico, x=peso, fill=nombre_cientifico)) +
    ggplot2::geom_col(position='dodge', alpha=0.8, colour='black') +
    ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::scale_x_continuous(expand = c(0, 2)) +
    ggplot2::scale_fill_brewer(palette = 'Paired') +
    qtheme() +
    ggplot2::guides('fill' = 'none') +
    ggplot2::labs(
      y = NULL,
      x = ifelse(lang() == 'ES','Peso muestreado (kg)','Sampled Weight (kg)'),
      fill=ifelse(lang() == 'ES','Especie','Species')
    )
  # Returning results as a list
  list(
    'top10_plot' = top10_plot,
    'prop_capt' = prop_capt_desc
  )
}

# Plotting gears most used (by weight) as a waffle/grid plot (proportional)
dg_plot_artes <- function(dbase, year_min, year_max, lang){
  # Filtering the data for the requested year range
  plotdf <- dbase[(year(fecha) >= year_min) & (year(fecha) < year_max), ] |> 
    # Translate and factorize gear names as required
    _[, tipo_arte := translate_gear(tipo_arte, lang())]
  
  # Creating waffle chart with ggplot
  plotdf |> 
    # Getting the total number of observations (rows) grouped by gear
    _[, .(peso = sum(peso, na.rm=T)), by=tipo_arte] |> 
    ggplot2::ggplot(ggplot2::aes(values = peso, fill=tipo_arte)) +
    waffle::geom_waffle(
      colour = "black", 
      make_proportional = T,
      n_rows = 10,
      linewidth=0.5
    ) +
    # Adding manual fill colours for the gears, based on the language
    ggplot2::scale_fill_manual(values = gear_colours(lang())) +
    theme_void() +
    qtheme() +
    theme(
      line = element_blank(), 
      rect = element_blank(),
      axis.text = element_blank(), 
      axis.title = element_blank(), 
      axis.ticks.length = unit(0, "pt"), 
      axis.minor.ticks.length = unit(0, "pt"), 
      panel.border = NULL
    ) +
    ggplot2::labs(
      y = NULL,
      fill=ifelse(lang() == 'ES','Arte','Gear')
    )
}

# Plotting community sampling effort for the current data
dg_plot_peso <- function(dbase, year_min, year_max, lang){
  # Filtering the data for the requested year range
  plotdf <- dbase[(year(fecha) >= year_min) & (year(fecha) < year_max), ] |> 
    # Translate and factorize gear names as required
    _[, tipo_arte := translate_gear(tipo_arte, lang())]
  
  # Plotting total weight by commmunity and gear
  plotdf |> 
    # Removing any data not associated with a community
    _[!is.na(comunidad),] |> 
    _[, `:=`(
      comunidad = factor(comunidad, levels=unique(comunidad))
    )] |> 
    _[, .(peso = sum(peso, na.rm=T)), by=.(comunidad, tipo_arte)] |> 
    ggplot2::ggplot(ggplot2::aes(y = tipo_arte, x=peso, fill=tipo_arte)) +
    ggplot2::geom_col(position='dodge', alpha=0.8, colour='black') +
    ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::scale_x_continuous(expand = c(0, 2)) +
    # Adding manual fill colours for the gears, based on the language
    ggplot2::scale_fill_manual(values = gear_colours(lang())) +
    ggplot2::facet_wrap('comunidad', ncol=1) +
    qtheme() +
    ggplot2::guides('fill' = 'none') +
    ggplot2::labs(
      y = NULL,
      x = ifelse(lang() == 'ES','Peso muestreado (kg)','Sampled Weight (kg)'),
      fill=ifelse(lang() == 'ES','Arte','Gear')
    )
}

# Plotting catch seasonality by community
dg_plot_ssn <- function(dbase, year_min, year_max, lang){
  # Filtering the data for the requested year range
  plotdf <- dbase[(year(fecha) >= year_min) & (year(fecha) < year_max), ]
  # Getting the correct month reference vector
  mref_vec <- if(lang() == 'ES') month.abb.esp else (month.abb)
  # Plotting total weight by commmunity and gear
  plotdf |> 
    # Removing any data not associated with a community
    _[!is.na(comunidad),] |> 
    _[, month := lubridate::month(fecha)] |> 
    _[, `:=`(
      comunidad = factor(comunidad, levels=unique(comunidad)),
      month = factor(mref_vec[month], levels=mref_vec)
    )] |> 
    _[, .(peso = sum(peso, na.rm=T)), by=.(month, comunidad)] |> 
    ggplot2::ggplot(ggplot2::aes(x = month, y=peso, colour=comunidad, group=comunidad)) +
    # ggplot2::geom_col(position='dodge', alpha=0.8, colour='black') +
    ggplot2::geom_line(alpha=0.6, show.legend = F) +
    ggplot2::geom_point(alpha=0.8, show.legend = F) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_fill_discrete(drop=FALSE) +
    ggplot2::facet_wrap('comunidad', ncol=1) +
    qtheme() +
    ggplot2::labs(
      x = NULL,
      y = ifelse(lang() == 'ES','Peso muestreado (kg)','Sampled Weight (kg)'),
      colour = ifelse(lang() == 'ES','Comunidad','Community')
    ) 
}

#' Timeline of key management milestones for the selected database
#'
#' Milestones are read from the `timeline_milestones` package dataset (built
#' from data-raw/app_metadata/timeline_milestones.csv), filtered to the
#' selected database and ordered by `sort_order`. Different databases show different timelines
#' with different period granularity: Tela's periods are years (2015-2019),
#' while Trujillo's are months within a single year (July-October).
#'
#' The bold "period" label above each node (a year or a month) is bilingual
#' via the `period_en`/`period_es` columns. A blank period means the node
#' continues under the previous period (e.g. a second milestone in the same
#' month) - it renders a marker and caption but no repeated period label.
#'
#' @param database The selected database name, e.g. 'Tela' or 'Trujillo'
#'   (matches the `database` column of `timeline_milestones`).
#' @param lang One of 'ES' or 'EN'
#' @returns an htmltools tag structure rendering the milestone timeline
dg_timeline <- function(curr_dbase, lang) {

  # --- Pull milestones for this database, ordered left-to-right ---
  
  # `timeline_milestones` is lazy-loaded package data (columns: database,
  # sort_order, period_en, period_es, label_en, label_es)
  ms <- timeline_milestones[database == curr_dbase][order(sort_order)]

  # --- Empty state: database has no milestones defined yet ---
  if (nrow(ms) == 0) {
    msg <- if (lang == 'ES') {
      'No hay hitos disponibles para esta base de datos.'
    } else {
      'No milestones available for this database.'
    }
    return(htmltools::div(class = "dg-timeline-empty", msg))
  }

  # --- Pick the period + caption columns for the active language ---
  if (lang == 'ES') {
    ms[, `:=`(period = period_es, caption = label_es)]
  } else {
    ms[, `:=`(period = period_en, caption = label_en)]
  }

  # --- Build one node per milestone ---
  nodes <- lapply(seq_len(nrow(ms)), function(i) {
    # A blank/NA period renders an empty (but space-reserving) slot so the
    # marker still lines up on the track.
    period <- ms$period[i]
    if (is.na(period)) period <- ""
    htmltools::div(
      class = "dg-timeline-node",
      htmltools::div(class = "dg-timeline-period", period),
      htmltools::div(class = "dg-timeline-marker"),        # the square dot
      htmltools::div(class = "dg-timeline-caption", ms$caption[i])
    )
  })

  # --- Assemble: a line + the nodes ---
  htmltools::div(
    class = "dg-timeline",
    htmltools::div(class = "dg-timeline-track"),           # the horizontal arrow line
    htmltools::div(class = "dg-timeline-nodes", nodes)
  )
}


