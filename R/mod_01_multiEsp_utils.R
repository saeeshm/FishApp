# Author: Saeesh Mangwani
# Date: 2024-07-30

# Description: Utility functions for the multi-species indicator page
# (essentially all the functions to generate the required plot based on reactive
# inputs)

#' @import data.table
#' @import ggplot2

# ==== Functions ====

# Custom plotting theme
qtheme <- function(size=15, font='serif'){
  ggplot2::theme_minimal(size, font) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill='white', colour=NA),
      # legend.position = 'bottom',
      panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5)
    )
}

# General function that deals with the reactive input and calls the relevant
# plotting function
# make_ms_plot <- function(lfqdbase, year_min, year_max){
# 
# }

# Length-frequency by trophic level plot
multiesp_lfq_plot <- function(lfqdbase, year_min, year_max){
  # Setting gears to factor to make sure none are dropped
  lfqdbase$tipo_arte <- factor(lfqdbase$tipo_arte)
  # Filtering the data for the requested year range
  plotdf <- lfqdbase[(year(fecha) >= year_min) & (year(fecha) < year_max), ]
  # Plotting
  plotdf |> 
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      ggplot2::aes(
        x = long_bin,
        fill=troph,
        group=troph,
        weight=adjWt
      ), 
      colour='white', 
      linewidth=0.01
    ) +
    ggplot2::scale_fill_distiller(palette = 'RdYlGn', direction=-1) +
    ggplot2::facet_wrap('tipo_arte', scales = 'fixed', drop=F, ncol=1) +
    qtheme() +
    ggplot2::labs(
      y = 'Peso capturado (kg)',
      x = 'Longitud (cm)',
      fill='Nivel\nTrófico'
    )
}

# Top-10 species captured plot
multiesp_top10_plot <- function(lfqdbase, year_min, year_max){
  # Filtering the data for the requested year range
  plotdf <- lfqdbase[(year(fecha) >= year_min) & (year(fecha) < year_max), ]
  ctab <- plotdf |> 
    # Calculating total catches by weight across the period
    _[, .(adjWt = sum(adjWt, na.rm=T)), by=nombre_cientifico] |> 
    # Picking the top 10
    _[order(adjWt),] |> 
    tail(15)
  
  # Filtering full data for just this and plotting
  ctab |> 
    # Factorizing
    _[, nombre_cientifico := factor(nombre_cientifico, levels=ctab$nombre_cientifico)] |> 
    ggplot2::ggplot(ggplot2::aes(y = nombre_cientifico, 
                                 fill=nombre_cientifico, 
                                 x=adjWt)) +
    ggplot2::geom_col(
      colour='white', 
      alpha=0.8, 
      show.legend = F
    ) +
    # ggplot2::scale_fill_brewer(palette = 'RdYlBu', direction=-1) +
    # ggplot2::scale_x_continuous(limits=c(0, 700)) +
    qtheme() +
    ggplot2::labs(
      x = 'Peso capturado (kg)',
      y= NULL,
    )
}

# Helper function for calculating log-sized bins across a given numeric range
calc_log_bins <- function(lmin, lmax, logBase){
  stopifnot(logBase >= 1)
  # Using the base, creating a large sequence of its exponent values (enough to
  # exceed the length range)
  numVals <- lmax/logBase
  binBreaks <- round(logBase^seq(1:numVals), 2)
  # Removing breaks which fall outside the range of the data
  keepidx <- which((binBreaks >= lmin) & (binBreaks <= lmax))
  # Adding one at the tail to capture the last group which gets removed
  keepidx[length(keepidx)+1] <- (tail(keepidx,1)+1)
  # Picking only the needed breaks, i.e the ones that cover the length range
  binBreaks <- binBreaks[keepidx]
  # Calculating bin-widths (size 1 less than the number of breaks)
  binWidths <- purrr::discard(binBreaks - data.table::shift(binBreaks, 1, type='lag'), is.na)
  # Calculating the mid-points between each bin
  binMids <- round(purrr::discard(binBreaks + (c(binWidths, NA)/2), is.na), 1)
  # Saving all in an output list
  outList <- list(
    'breaks' = binBreaks,
    'mids' = binMids
  )
}

# Normalized Biomass density plot
multiesp_bspect_plot <- function(fishdbase, lfqdbase, year_min, year_max){
  # Filtering the data for the requested year range
  plotdf <- fishdbase[(year(fecha) >= year_min) & (year(fecha) < year_max), ]
  
  # Min and max sizes (in grams) - uses the full database
  lmin <- floor(min(fishdbase$peso, na.rm=T))
  lmax <- ceiling(max(fishdbase$peso, na.rm=T))
  
  # Grouping sizes into Log-width bins, returning a list containing the breaks,
  # widths, and labels
  logBase <- 1.5
  logBins <- calc_log_bins(lmin-1, lmax, logBase)
  
  # Calculating normalized biomass density by bin for this time period of data
  lldat <- plotdf |> 
    _[, adjWt := round(peso, 0)] |> 
    _[, lClass := cut(adjWt, breaks = logBins$breaks, labels=F)] |> 
    _[, .(biomass = sum(adjWt, na.rm=T)), by = 'lClass'] |> 
    _[!is.na(lClass)] |> 
    setkey(lClass) |> 
    _[, `:=`(
      biomass = biomass,
      bin_start = logBins$breaks[lClass],
      bin_end = logBins$breaks[-1][lClass]
    )] |> 
    _[, `:=`(
      bin_width = bin_end - bin_start,
      bin_midpoint = exp((log(bin_start) + log(bin_end)) / 2)
    )] |> 
    _[ , biomass_density := biomass/bin_width]
  
  # Filtering only densities after the peak for fitting a linear model
  # Identifying where the biomass density peaks
  peak_idx <- which.max(log(lldat$biomass_density))
  peak <- floor(lldat$bin_midpoint[peak_idx])
  mdat <- lldat[bin_midpoint >= peak, ]
  mdat_mod <- mdat |> 
    _[, .(
      lclass_int = 1:length(unique(lClass)),
      bin_midpoint = log(bin_midpoint, base=logBase),
      biomass_density = log(biomass_density, base=logBase)
    )]
  
  # Fitting linear regression on descending slope
  m <- lm(biomass_density ~ bin_midpoint, data=mdat_mod)
  
  # Creating regression annotation
  annotation <- paste0(
    'B: ', round(m$coefficients[2], 2), '\n',
    'SE(B): ', round(sqrt(diag(vcov(m)))[2], 2)
    # 'R^2: ', round(summary(mod$m)$adj.r.squared, 2)
  )
  
  # Getting axis limits based on the data, to adjust the position of the
  # regression box
  y_lab_pos <-  mdat$biomass_density[1] + mdat$biomass_density[1]*0.2
  x_lab_pos <- mdat$bin_midpoint[length(mdat$bin_midpoint) - 3.5]
  
  # Creating plot with overlaid regression
  lldat |> 
    ggplot2::ggplot(ggplot2::aes(x = bin_midpoint, y = biomass_density)) +
    ggplot2::geom_line(colour='grey50') +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(
      # limits=c(2, 12000),
      transform = scales::log_trans(base=logBase), 
      name = "Weight (grams)",
      # Axis labels to have a precision of only 2 decimal places
      labels = function(x) sprintf("%.2f", x)
    ) + 
    ggplot2::scale_y_continuous(
      # limits = c(1, 6000),
      transform = scales::log_trans(base=logBase), name = "Normalized Biomass Density",
      labels = function(x) sprintf("%.2f", x)
    ) +
    qtheme() +
    # Adding regression line
    ggplot2::geom_smooth(
      data=mdat,
      ggplot2::aes(x = bin_midpoint, y=biomass_density),
      method = 'lm',
      linewidth=0.6,
      # colour='black',
      colour='firebrick',
      alpha=0.4
    ) +
    # Adding regression details
    ggplot2::annotate(
      geom='label',
      label=annotation,
      x=x_lab_pos,
      y=y_lab_pos,
      family='serif',
      hjust=0,
      vjust=1.2,
      size=3.5,
      label.padding=ggplot2::unit(0.6, "lines")
    )
}

# CPUE plot
multiesp_cpue_plot <- function(lfqdbase, year_min, year_max){
  # Filtering the data for the requested year range
  plotdf <- lfqdbase[(year(fecha) >= year_min) & (year(fecha) < year_max), ]
  
  # Plotting
  plotdf |> 
    _[, `:=`(
      cpue = adjWt/tot_boats,
      year = lubridate::year(fecha),
      month = lubridate::month(fecha)
    )] |> 
    _[, fecha2 := lubridate::ymd(paste(year, month, '01', sep='-'))] |> 
    _[, .(cpue = mean(cpue, na.rm=T)), by=fecha2] |> 
    ggplot2::ggplot(ggplot2::aes(x = fecha2, y=cpue)) +
    ggplot2::geom_point(alpha=0.4, show.legend = F) +
    ggplot2::geom_line(alpha=0.8, show.legend = F) +
    ggplot2::scale_color_brewer(palette = 'Set1') +
    ggplot2::geom_smooth(method='lm', colour='firebrick', linewidth=0.5) +
    # ggpmisc::stat_poly_eq(ggpmisc::use_label(c("P", "R2")), 
    #                       family='sans', size=4, label.x = 'left') +
    qtheme() +
    ggplot2::labs(
      x = NULL,
      y = 'CPUE (kg/dia)'
    )
}
