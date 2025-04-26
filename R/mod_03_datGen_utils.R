# Author: Saeesh Mangwani
# Date: 2024-08-14

# Description: Utility functions for the sampling information page (essentially
# all the functions to generate the required plot based on reactive inputs)

#' @import data.table
#' @import ggplot2

# ==== Functions ====

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
    # Total weight sampled
    peso_muestreado = round(sum(currdf$peso, na.rm=T)/1000, 0),
    # Total number of species sampled
    num_especies = uniqueN(currdf$nombre_cientifico),
    # Total number of individuals sampled
    num_individuos = nrow(currdf)
  )
}

# Plotting total weight sampled per gear for the current data
dg_plot_por_arte <- function(dbase, year_min, year_max){
  # Filtering the data for the requested year range
  currdf <- dbase[(year(fecha) >= year_min) & (year(fecha) < year_max), ]
  
  # Preparing data for plotting
  plotdf <- currdf |> 
    _[, tipo_arte := ifelse(is.na(tipo_arte), 'Desconocido', tipo_arte)] |> 
    _[, `:=`(tipo_arte = factor(tipo_arte, levels=c('Desconocido', uartes)))] |> 
    _[, .(peso = sum(peso, na.rm=T)/1000), by=.(tipo_arte)]
  
  # Plotting
  plotdf |> 
    ggplot2::ggplot(ggplot2::aes(y = tipo_arte, x=peso, fill=tipo_arte)) +
    ggplot2::geom_col(position='dodge', alpha=0.8, colour='black') +
    # coord_polar(start=0) +
    ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::scale_x_continuous(expand = c(0, 2)) +
    ggplot2::scale_fill_discrete(drop=FALSE) +
    qtheme() +
    ggplot2::guides('fill' = 'none') +
    ggplot2::labs(
      x = 'Arte',
      y = 'Peso muestreado (kg)',
      fill='Arte'
    )
}

# Plotting total weight sampled for the 10 most commonly caught species (by
# weight) from the current data
dg_plot_esp_comunes <- function(dbase, year_min, year_max, lang){
  # Filtering the data for the requested year range
  currdf <- dbase[(year(fecha) >= year_min) & (year(fecha) < year_max), ]
  
  # Getting the 10 most commonly caught species
  plotdf <- currdf |> 
    _[, .(peso = sum(peso, na.rm=T)/1000), by=.(nombre_cientifico)] |> 
    _[, .(peso = sum(peso)), by=.(nombre_cientifico)] |> 
    _[order(-peso)] |> 
    head(10)
  
  # What proportion of weight do these 10 species comprise, relative to the
  # total caught weight
  prop_capt <- (sum(plotdf$peso)/(sum(currdf$peso, na.rm=T)/1000)) * 100
  
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
    'prop_capt' = round(prop_capt, 0)
  )
}

# Plotting gears most used (by weight) as a waffle/grid plot (proportional)
dg_plot_artes <- function(dbase, year_min, year_max, lang){
  # Filtering the data for the requested year range
  plotdf <- dbase[(year(fecha) >= year_min) & (year(fecha) < year_max), ]
  # All unique combinations of gears
  uartes <- na.omit(unique(plotdf$tipo_arte))
  
  # Creating waffle chart with ggplot
  plotdf |> 
    # Classifying unknown gear type data, and factorizing
    _[, tipo_arte := ifelse(is.na(tipo_arte), 'Desconocido', tipo_arte)] |> 
    _[, `:=`(
      tipo_arte = factor(tipo_arte, levels=c('Desconocido', uartes))
    )] |> 
    # Getting the total number of observations (rows) grouped by gear
    _[, .N, by=tipo_arte] |> 
  # test |> 
    ggplot2::ggplot(ggplot2::aes(values = N, fill=tipo_arte)) +
    waffle::geom_waffle(
      colour = "black", 
      make_proportional = T,
      n_rows = 10,
      linewidth=0.5
    ) +
    # coord_equal() +
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
      fill=ifelse(lang() == 'ES','Arte','Technique')
    )
}

# Plotting community sampling effort for the current data
dg_plot_peso <- function(dbase, year_min, year_max, lang){
  # Filtering the data for the requested year range
  plotdf <- dbase[(year(fecha) >= year_min) & (year(fecha) < year_max), ]
  # All unique combinations of gears
  uartes <- na.omit(unique(plotdf$tipo_arte))
  
  # Plotting total weight by commmunity and gear
  plotdf |> 
    # Removing any data not associated with a community
    _[!is.na(comunidad),] |> 
    _[, tipo_arte := ifelse(is.na(tipo_arte), 'Desconocido', tipo_arte)] |> 
    _[, `:=`(
      comunidad = factor(comunidad, levels=unique(comunidad)),
      tipo_arte = factor(tipo_arte, levels=c('Desconocido', uartes))
    )] |> 
    _[, .(peso = sum(peso, na.rm=T)/1000), by=.(comunidad, tipo_arte)] |> 
    ggplot2::ggplot(ggplot2::aes(y = tipo_arte, x=peso, fill=tipo_arte)) +
    ggplot2::geom_col(position='dodge', alpha=0.8, colour='black') +
    ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::scale_x_continuous(expand = c(0, 2)) +
    ggplot2::scale_fill_discrete(drop=FALSE) +
    ggplot2::facet_wrap('comunidad', ncol=1) +
    qtheme() +
    ggplot2::guides('fill' = 'none') +
    ggplot2::labs(
      y = NULL,
      x = ifelse(lang() == 'ES','Peso muestreado (kg)','Sampled Weight (kg)'),
      fill=ifelse(lang() == 'ES','Arte','Technique')
    )
}

# Plotting catch seasonality by community
dg_plot_ssn <- function(dbase, year_min, year_max, lang){
  # Filtering the data for the requested year range
  plotdf <- dbase[(year(fecha) >= year_min) & (year(fecha) < year_max), ]
  
  # Plotting total weight by commmunity and gear
  plotdf |> 
    # Removing any data not associated with a community
    _[!is.na(comunidad),] |> 
    _[, tipo_arte := ifelse(is.na(tipo_arte), 'Desconocido', tipo_arte)] |> 
    _[, month := lubridate::month(fecha)] |> 
    _[, `:=`(
      comunidad = factor(comunidad, levels=unique(comunidad)),
      month = factor(month.abb[month], levels=month.abb)
    )] |> 
    _[, .(peso = sum(peso, na.rm=T)/1000), by=.(month, comunidad)] |> 
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
