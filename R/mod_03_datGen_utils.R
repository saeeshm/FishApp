# Author: Saeesh Mangwani
# Date: 2024-08-14

# Description: Utility functions for the sampling information page (essentially
# all the functions to generate the required plot based on reactive inputs)

#' @import data.table
#' @import ggplot2

# ==== Functions ====

# Plotting community sampling effort for the current data
dg_plot_peso <- function(dbase, year_min, year_max){
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
      x = 'Peso muestreado (kg)',
      fill='Arte'
    )
}

# Plotting catch seasonality by community
dg_plot_ssn <- function(dbase, year_min, year_max){
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
      y = 'Peso muestreado (kg)',
      colour='Comunidad'
    )
}
