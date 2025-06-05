# Author: Saeesh Mangwani
# Date: 2024-07-30

# Description: Utility functions for the multi-species indicator page
# (essentially all the functions to generate the required plot based on reactive
# inputs)

#' @import data.table
#' @import ggplot2

# ==== Functions ====

# Calculating species richness, diversity and evenness indices for the given
# data
ms_div_indices <- function(lfqdbase, year_min, year_max){
  # Filtering the data for the requested year range
  plotdf <- lfqdbase[(lubridate::year(fecha) >= year_min) & (lubridate::year(fecha) < year_max), ]
  # Getting a count of all unique species in this dataset (species richness)
  rich <- length(unique(plotdf$nombre_cientifico))
  
  # Total weight caught this period
  totwt <- sum(plotdf$adjWt, na.rm=T)
  # Calculating the proportional catch of each species by weight
  pspec <- plotdf |> 
    _[, .(propwt = sum(adjWt, na.rm=T)/totwt), by=nombre_cientifico]
  # Maximum possible shannon index value with the current species mix (occurs
  # when species distribution is perfectly even - used as a reference for the
  # equitability index
  even_pspec <- rep(1/rich, rich)
  hmax <- -1 * sum(even_pspec * log(even_pspec))
  
  # Shannon diversity index
  shannon <- -1 * sum(pspec$propwt * log(pspec$propwt))
  # Equitability index
  eq <- shannon/hmax
  
  # Returning all indices as a list
  return(list(
    'rich' = round(rich, 3),
    'shannon' = round(shannon, 3),
    'eq' = round(eq, 3)
  ))
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
ms_bdens_plot <- function(fishdbase, year_min, year_max, lang){
  # Filtering the data for the requested year range
  plotdf <- fishdbase[(lubridate::year(fecha) >= year_min) & (lubridate::year(fecha) < year_max), ]
  
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
  
  # Extracting slope and confidence interval
  slope_estimate <- round(m$coefficients[2], 3)
  slope_ci <- round(confint(m, level=0.95)[2,], 3)
  # Making description text
  bd_desc <- paste(
    ifelse(lang == 'ES','Pendiente:','Slope:'),
    paste0(
      slope_estimate, 
      ' (', 
      slope_ci[1], 
      ', ', 
      slope_ci[2], 
      ')'
    )
  )
  
  # Creating plot with overlaid regression line
  bd_plot <- lldat |> 
    ggplot2::ggplot(ggplot2::aes(x = bin_midpoint, y = biomass_density)) +
    ggplot2::geom_line(colour='grey50') +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(
      # limits=c(2, 12000),
      transform = scales::log_trans(base=logBase), 
      name = ifelse(lang == 'ES',"Talla (gramos)","Size (grams)"),
      # Axis labels to have a precision of only 2 decimal places
      labels = function(x) sprintf("%.2f", x)
    ) + 
    ggplot2::scale_y_continuous(
      # limits = c(1, 6000),
      transform = scales::log_trans(base=logBase), 
      name = ifelse(lang == 'ES',"Densidad de biomasa","Biomass Density"),
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
    )
  
  # Returning plot and biomass density regression coefficients
  return(list(
    'bd_plot' = bd_plot,
    'bd_desc' = bd_desc
  ))
}

# Multispecies CPUE plot
ms_cpue_plot <- function(lfqdbase, year_min, year_max,lang){
  # Filtering the data for the requested year range
  plotdf <- lfqdbase[(lubridate::year(fecha) >= year_min) & (lubridate::year(fecha) < year_max), ]
  
  # Preparing CPUE dataframe for summarization
  cpueDat <- plotdf |> 
    _[, `:=`(
      cpue = adjWt/tot_boats,
      year = lubridate::year(fecha),
      month = lubridate::month(fecha)
    )] |> 
    _[, fecha2 := lubridate::ymd(paste(year, month, '01', sep='-'))] |> 
    _[, .(cpue = mean(cpue, na.rm=T)), by=.(fecha2)]
  
  # Fitting time-trend linear model
  modDat <- cpueDat |> 
    _[, `:=`(
      fecha2_days = difftime(fecha2, fecha2[1], units='days')
    )]
  lm_cpue <- lm(cpue ~ fecha2_days, data=modDat)
  
  # Extract the slope estimate and confidence interval (in per year units)
  slope_estimate <- round(coef(lm_cpue)[2]*365, 3)
  conf_interval <- round(confint(lm_cpue)[2, ]*365, 3)
  # Making description text
  cpue_desc <- paste(
    ifelse(lang == 'ES','Tendencia:','Trend:'),
    paste0(
      slope_estimate, 
      ' (', 
      conf_interval[1], 
      ', ', 
      conf_interval[2], 
      ')',
      ifelse(lang == 'ES',' kg/año',' kg/year')
    )
  )
  
  # Plotting overall cpue trend
  cpue_plot <- cpueDat |> 
    ggplot2:: ggplot(ggplot2::aes(x = fecha2, y=cpue)) +
    ggplot2::geom_point(alpha=0.4, show.legend = F) +
    ggplot2::geom_line(alpha=0.8, show.legend = F) +
    # ggplot2::scale_y_continuous(limits = c(0, 5)) +
    ggplot2::geom_smooth(method='lm', colour='#00AFA4', fill='#00AFA410', linewidth=0.6) +
    # ggpmisc::stat_poly_eq(use_label(c("P", "R2"))) +
    qtheme() +
    ggplot2::labs(
      x = NULL,
      y = ifelse(lang == 'ES','CPUE (kg/dia)','CPUE (kg/day)')
    )
  # Returning all items as a list
  return(list(
    'cpue_plot' = cpue_plot,
    'cpue_desc' = cpue_desc
  ))
}

# Multi-species length-frequency by trophic level and gear plot
ms_lfq_plot <- function(lfqdbase, year_min, year_max, lang){
  # Setting gears to factor to make sure none are dropped
  lfqdbase$tipo_arte <- factor(lfqdbase$tipo_arte)
  # Filtering the data for the requested year range
  plotdf <- lfqdbase[(lubridate::year(fecha) >= year_min) & (lubridate::year(fecha) < year_max), ]
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
      y = ifelse(lang == 'ES', 'Peso capturado (kg)','Catch Weight (kg)'),
      x = ifelse(lang == 'ES', 'Longitud (cm)', 'Length (cm)'),
      fill=ifelse(lang == 'ES', 'Nivel\nTrófico', 'Trophic\nLevel')
    )
}

# Top-10 species captured as deviances from mean capture plot
ms_top10_plot <- function(lfqdbase, year_min, year_max, lang){
  # Filtering the data for the requested year range
  plotdf <- lfqdbase[(lubridate::year(fecha) >= year_min) & (lubridate::year(fecha) < year_max), ]
  ctab <- plotdf |> 
    # Calculating total catches by weight across the period
    _[, .(adjWt = sum(adjWt, na.rm=T)), by=c('nombre_comun_cln')] |> 
    # Picking the top 10
    _[order(adjWt),] |> 
    _[, nombre_comun_cln := stringr::str_to_title(nombre_comun_cln)] |> 
    tail(10)
  ctab <- ctab |>
    # Factorizing, so the data appear in ascending order of weight captured
    _[, nombre_comun_cln := factor(nombre_comun_cln, levels=ctab$nombre_comun_cln)] 
  
  # Calculating the mean weight of capture from among all these species
  meanWt <- mean(ctab$adjWt, na.rm=T)
  
  # Calculating deviances from the mean
  ctab[, devWt := adjWt - meanWt]
  # Getting the maximum deviance value available to set even limits on the scale
  # range
  maxDevWt <- max(abs(ctab$devWt), na.rm=T)
  
  # Plotting data as mean and deviances from the mean
  dev_plot <- ctab |> 
    ggplot2::ggplot(
      ggplot2::aes(y = nombre_comun_cln, fill=devWt, x=devWt)
    ) +
    ggplot2::geom_col(
      colour='white', 
      alpha=0.8, 
      show.legend = F
    ) +
    ggplot2::geom_vline(
      xintercept = 0, 
      colour='black', 
      linewidth=0.5, 
      linetype='dashed'
    ) +
    ggplot2::scale_fill_steps2(low='#fc7a64', high='#fc7a64', mid='#bfdc5d', limits=c(-maxDevWt, maxDevWt)) +
    # ggplot2::scale_fill_brewer(palette = 'RdYlBu', direction=-1) +
    # ggplot2::scale_x_continuous(limits=c(0, 700)) +
    qtheme() +
    ggplot2::labs(
      x = ifelse(lang == 'ES','Desviación del peso capturado con respecto a la media (kg)','Deviance of captured weight relative to mean (kg)'),
      y= NULL,
    )
  # Returning the plot
  return(dev_plot)
  # Returning the deviance plot and the mean captured weight
  # return(list(
  #   'dev_plot' = dev_plot,
  #   'meanWt' = round(meanWt, 3)
  # ))
}

