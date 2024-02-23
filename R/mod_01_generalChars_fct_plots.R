# Author: Saeesh Mangwani
# Date: 2024-02-15

# Description: A general function that filters the data based on provided
# inputs, calls the appropriate function to produce the requested GGplot for the
# General Characteristics page.

#' @import data.table

# ==== Function definition ====
make_plot <- function(fishdbase, plot_type, y_unit, comunidad, sample_year){
  # Filtering relevant data based on filter inputs - getting the plotting
  # dataframe
  plotdf <- if(comunidad=='Todo'){
    fishdbase[lubridate::year(fecha) == sample_year, ]
    # fishdbase[(fecha >= time_min) & (fecha < time_max), ]
  }else{
    fishdbase[(comunidad == comunidad) & (lubridate::year(fecha) == sample_year), ]
  }
  
  # Using the index list to call the appropriate function to generate the
  # requested plot-type
  .plotFuncList[[plot_type]](plotdf, y_unit)
}

# Produces an average length per species plot (10-most caught)
.length_plot <- function(plotdf, y_unit){
  if(y_unit == "Weight"){
    # If using weight, calculating total caught-weight in kg per gear as well as
    # average length for every species name
    currdf <- plotdf[, .(
      peso_kg = sum(peso, na.rm=T)/1000,
      long_prom = mean(longitud, na.rm=T)
    ), by=nombre_comun_cln]
    # Selecting top-10 by weight
    currdf <- head(currdf[order(-peso_kg)], 10)
    .my_col_plot(currdf, x='nombre_comun_cln', y='long_prom', 
                 y_lab = 'Talla promedio [cm]')
  }else{
    # Calculating number of samples per species
    currdf <- plotdf[, .(
      'N' = .N,
      long_prom = mean(longitud, na.rm=T)
    ), by=nombre_comun_cln]
    # Selecting top-10
    currdf <- head(currdf[order(-N)], 10)
    # Arranging by number and selecting top-10
    .my_col_plot(currdf, x='nombre_comun_cln', y='long_prom',
                 y_lab = 'Talla promedio [cm]')
  }
}

# Produces a 10-most caught species plot
.species_plot <- function(plotdf, y_unit){
  if(y_unit == "Weight"){
    # If using weight, calculating total caught-weight in kg per gear
    currdf <- plotdf[, .(peso_kg = sum(peso, na.rm=T)/1000), by=nombre_comun_cln]
    # Selecting top-10
    currdf <- head(currdf[order(-peso_kg)], 10)
    .my_col_plot(currdf, x='nombre_comun_cln', y='peso_kg')
  }else{
    # Calculating number of samples per species
    currdf <- plotdf[, .N, by=nombre_comun_cln]
    # Selecting top-10
    currdf <- head(currdf[order(-N)], 10)
    # Arranging by number and selecting top-10
    .my_col_plot(currdf, x='nombre_comun_cln', y='N', y_lab='Número de muestras')
  }
}

# Produces a total catches by gear plot
.gear_plot <- function(plotdf, y_unit){
  if(y_unit == "Weight"){
    # If using weight, calculating total caught-weight in kg per gear
    currdf <- plotdf[, .(peso_kg = sum(peso, na.rm=T)/1000), by=tipo_arte]
    .my_col_plot(currdf, x='tipo_arte', y='peso_kg')
  }else{
    .my_bar_plot(plotdf, x='tipo_arte')
  }
}

# Produces a seasonality plot (uses a fixed y-unit of weight)
.seasonality_plot <- function(plotdf, y_unit){
  # Adding a month column
  currdf <- plotdf[, month := factor(month.abb[lubridate::month(ym)], levels=month.abb)]
  # Plotting
  if(y_unit=='Weight'){
    # If using weight, calculating total caught-weight in kg per month
    currdf <- plotdf[, .(peso_kg = sum(peso, na.rm=T)/1000), by=month]
    .my_col_plot(currdf, x='month', y='peso_kg')
  }else{
    .my_bar_plot(currdf, x='month')
  }
}

# Generic bar-plot template - used for plotting all the "number" plots above
.my_bar_plot <- function(plotdf, x, y_lab='Número de muestras'){
  plotdf |> 
    ggplot2::ggplot(ggplot2::aes(
      # User-defined variable name
      x = !!rlang::sym(x), 
      fill=!!rlang::sym(x)
    )) +
    # Defaults to stat=count
    ggplot2::geom_bar(colour='black', alpha=0.8, show.legend = F) +
    ggplot2::scale_x_discrete(drop=F) +
    ggplot2::scale_y_continuous(expand = c(0,0,0.05, 0)) +
    fishTheme() +
    ggplot2::labs(
      x = NULL,
      # User defined y-axis label
      y = y_lab
    )
}

# Generic column-plot template - used for plotting all the "weight" plots above
# (the weight variable needs to be created before passing to the y-argument
# here)
.my_col_plot <- function(plotdf, x, y, y_lab='Peso muestreado [kg]'){
  plotdf |> 
    ggplot2::ggplot(ggplot2::aes(
      # User defined x and y arguments
      x = !!rlang::sym(x), 
      y = !!rlang::sym(y),
      fill = !!rlang::sym(x), 
    )) +
    ggplot2::geom_col(colour='black', alpha=0.8, show.legend = F) +
    ggplot2::scale_x_discrete(drop=F) +
    ggplot2::scale_y_continuous(expand = c(0,0,0.05, 0)) +
    fishTheme() +
    ggplot2::labs(
      x = NULL,
      # User defined y-axis label
      y = y_lab
    )
}

# Named list of all helper plotting functions (used to index the correct
# function in the main make_plot call)
.plotFuncList <- list(
  'seasonality' = .seasonality_plot, 
  'gear' = .gear_plot,
  'species' = .species_plot,
  'length' = .length_plot
)
