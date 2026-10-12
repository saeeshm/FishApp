# Author: Saeesh Mangwani
# Date: 2024-08-09

# Description: Utility functions for the single-species indicator page
# (essentially all the functions to generate the required plot based on reactive
# inputs)

#' @import data.table
#' @import ggplot2

# ==== Functions ====

# Reference objects ----------

# A spanish version of the month.abb vector
month.abb.esp <- c(
  'Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun',
  'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic'
)

# Data filtering ----------

# Creating a plotting dataset by filtering for the provided species
make_plotdf <- function(dbase, spectab, year_min, year_max, currspec){
  # Getting the scientific name associated with this common name
  ncien <- spectab[nombre_comun_cln %chin% stringr::str_to_lower(currspec)]$nombre_cientifico
  # Filtering the database for data in this year period for this species
  dbase[(year(fecha) >= year_min) & 
          (year(fecha) < year_max) & 
          (nombre_cientifico %chin% ncien), ]
}

# Basic summary stats and reference figures ----------

# Calculate L-infinity from the maximum length available in the database, using
# Froese and Binholan's (2000) empirical equation
calc_linf <- function(Lvec, rm_outliers=T){
  # Removing outliers if requested
  if(rm_outliers){
    isanom <- anomalize::iqr(Lvec, alpha=0.05)
    outLens <- Lvec[isanom == 'No']
  }else{
    outLens <- Lvec
  }
  # Calculating L-infinity using the equation
  exp(0.044 + 0.9841*log(max(outLens, na.rm=T)))
}

# Calculates all relevant froese indicators given an estimate of Linf, using
# empirical equations following Froese 2004 and Frose and Binholan 2000
calc_froese_indics <- function(Linf){
  # Length of maturity
  Lmat <- exp((0.8979 * log(Linf)) - 0.0782)
  mat_range <- c(Lmat - Lmat*0.1, Lmat + Lmat*0.1)
  # Optimal length range 
  Lopt <- exp((1.0421 * log(Linf)) - 0.2742)
  opt_range <- c(Lopt - Lopt*0.1, Lopt + Lopt*0.1)
  # Megaspawner length
  LMspawn <- Lopt+(Lopt*0.1)
  return(list(
    'Linf' = Linf,
    'Lmat' = Lmat,
    'mat_range' = mat_range,
    'Lopt' = Lopt,
    'opt_range' = opt_range,
    'LMspawn' = LMspawn
  ))
}

# Calculating basic summary statistics for this species and time period
ss_summ_stats <- function(fishdbase, lfqdbase, spectab, year_min, year_max, currspec){
  # Getting the relevant data subsets
  curr_lfqdb <- make_plotdf(lfqdbase, spectab, year_min, year_max, currspec)
  curr_fdb <- make_plotdf(fishdbase, spectab, year_min, year_max, currspec)
  
  # Calculating L-infinity (using raw sampled lengths)
  Linf <- calc_linf(curr_fdb$longitud, rm_outliers = T)
  
  # Calculating Froese indicators
  froese <- calc_froese_indics(Linf)
  
  # Proportion of captured lengths within maturity range
  prop_mat <- curr_lfqdb |> 
    # Filtering for lengths within the maturity range
    _[long_bin >= froese$mat_range[1] & long_bin <= froese$mat_range[2], ] |> 
    # Summarizing the proportion of captures in this range
    _[, .(
      prop_matr = (sum(adjCount, na.rm=T)/sum(curr_lfqdb$adjCount, na.rm=T)) * 100
    )] |> 
    unlist()
  
  # Proportion of captured lengths that were immagure (below maturity range)
  prop_immat <- curr_lfqdb |>
    # Filtering for lengths below the maturity range
    _[long_bin < froese$mat_range[1], ] |>
    # Summarizing the proportion of captures in this range
    _[, .(
      prop_immat = (sum(adjCount, na.rm=T)/sum(curr_lfqdb$adjCount, na.rm=T)) * 100
    )] |>
    unlist()
  
  # Proportion of captures within the optimal length range
  # prop_opt <- curr_lfqdb |> 
  #   # Filtering for lengths within the optimal range
  #   _[long_bin >= froese$opt_range[1] & long_bin <= froese$opt_range[2], ] |> 
  #   # Summarizing the proportion of captures in this range
  #   _[, .(
  #     prop_opt = (sum(adjCount, na.rm=T)/sum(curr_lfqdb$adjCount, na.rm=T)) * 100
  #   )] |> 
  #   unlist()
  
  # Proportion of captures that were mega-spawners
  prop_mspawn <- curr_lfqdb |> 
    # Filtering for lengths within the mega-spawner range
    _[long_bin >= froese$LMspawn, ] |> 
    # Summarizing the proportion of captures in this range
    _[, .(
      prop_mspawn = (sum(adjCount, na.rm=T)/sum(curr_lfqdb$adjCount, na.rm=T)) * 100
    )] |> 
    unlist()
  
  # List of required summary stats
  list(
    # Total number of captures in this year period
    num_capturado = sum(curr_lfqdb$adjCount, na.rm=T),
    # Total weight captured (kg)
    peso_capturado = round(sum(curr_lfqdb$adjWt, na.rm = T), 0),
    # Proportion of captures within the maturity range
    prop_capturas_matr = paste(round(prop_mat, 2), '%'),
    # Proportion of captures that were immature (below maturity range)
    prop_capturas_immat = paste(round(prop_immat, 2), '%'),
    # Proportion of captures that were mega-spawners
    prop_capturas_mspawn = paste(round(prop_mspawn, 2), '%'),
    # All froese indicators for reference
    froese = froese
  )
}

# Plots ----------

# Overall CPUE trend
ss_cpue_trend_total <- function(lfqdbase, spectab, year_min, year_max, currspec, lang){
  # Getting plotting data
  plotdf <- make_plotdf(lfqdbase, spectab, year_min, year_max, currspec)
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
    setkey(fecha2) |>
    _[, `:=`(
      fecha2_days = difftime(fecha2, fecha2[1], units='days')
    )]
  lm_cpue <- lm(cpue ~ fecha2_days, data=modDat)
  # Extract the slope estimate and confidence interval (in per year units)
  slope_estimate <- round(coef(lm_cpue)[2]*365, 3)
  ci <- round(confint(lm_cpue)[2, ]*365, 2)
  if(all(is.nan(ci))){
    # If the confidence interval is NaN (i.e not enough data in a given year),
    # set it to NA
    ci <- c(NA, NA)
    signif <- F
  }else{
    # Is it significant?
    signif <- !((ci[1] <= 0) & (ci[2] >= 0))
  }
  
  # Calculating average CPUE for this time period
  avg_cpue <- round(mean(cpueDat$cpue, na.rm=T), 2)
  
  # Making description texts (adding markdown emphasis formatting) ----------
  
  # Colour choice for trend based on significance
  if(!signif){
    trend_col <- '#eddfca' # light-green for non-significant trends
  } else if(slope_estimate < 0){
    trend_col <- '#B22222' # Red for significant negative trends
  }else{
    trend_col <- '#006400' # Green for significant positive trends
  }
  
  # Trend description
  cpue_trend_desc <- p(
    strong(ifelse(lang == 'ES','Tendencia de CPUE: ','CPUE Trend: ')),
    em(
      paste0(slope_estimate, ' (', ci[1], ', ', ci[2], ') '), 
      style = paste0('color: ', trend_col),
    ),
    ifelse(lang == 'ES',' kg/año',' kg/year')
  )
  
  # Mean description
  cpue_mean_desc <- p(
    strong(ifelse(lang == 'ES','Media de CPUE: ','Mean CPUE: ')),
    em(avg_cpue, style = 'color: #3ebbed'),
    ifelse(lang == 'ES',' kg/dia',' kg/day')
  )
  
  # Plotting ----------
  
  # Plotting overall cpue trend
  cpue_plot <- cpueDat |> 
    ggplot2:: ggplot(ggplot2::aes(x = fecha2, y=cpue)) +
    ggplot2::geom_point(alpha=0.4, show.legend = F) +
    ggplot2::geom_line(alpha=0.8, show.legend = F) +
    # Adding trendline
    ggplot2::geom_smooth(method='lm', colour=trend_col, fill=trend_col, alpha=0.4, linewidth=1.1) +
    # Adding average reference line
    ggplot2::geom_hline(
      yintercept = avg_cpue, 
      colour='#3ebbed', 
      linewidth=1.1, 
      linetype='dashed'
    ) +
    # ggpmisc::stat_poly_eq(use_label(c("P", "R2"))) +
    qtheme() +
    ggplot2::labs(
      x = NULL,
      y = ifelse(lang == 'ES','CPUE (kg/dia)','CPUE (kg/day)')
    )
  # Returning all items as a list
  list(
    'cpue_plot' = cpue_plot,
    'cpue_trend_desc' = cpue_trend_desc,
    'cpue_mean_desc' = cpue_mean_desc
  )
}

# CPUE per gear
# ss_cpue_por_arte <- function(lfqdbase, spectab, year_min, year_max, currspec, lang){
#   # Getting plotting data
#   plotdf <- make_plotdf(lfqdbase, spectab, year_min, year_max, currspec)
#   # Preparing CPUE dataframe for summarization
#   cpueDat <- plotdf |> 
#     _[, `:=`(
#       cpue = adjWt/tot_boats,
#       year = lubridate::year(fecha),
#       month = lubridate::month(fecha)
#     )] |> 
#     _[, fecha2 := lubridate::ymd(paste(year, month, '01', sep='-'))]
#   
#   # If plotting by gear, summarizing by gear and month before plotting
#   cpueDat |> 
#     _[, .(cpue = mean(cpue, na.rm=T)), by=.(fecha2, tipo_arte)] |> 
#     ggplot2::ggplot(ggplot2::aes(x = fecha2, y=cpue)) +
#     ggplot2::geom_point(alpha=0.4, show.legend = F) +
#     ggplot2::geom_line(alpha=0.8, show.legend = F) +
#     # stat_poly_line() +
#     # stat_poly_eq(use_label(c("P", "R2"))) +
#     ggplot2::scale_y_continuous(limits = c(0, 3)) +
#     ggplot2::facet_wrap('tipo_arte', ncol=1) +
#     qtheme() +
#     ggplot2::labs(
#       x = NULL,
#       y = ifelse(lang == 'ES','CPUE (kg/dia)','CPUE (kg/day)')
#     )
# }

# Plotting length-frequencies by gear for this species
ss_lfq_plot <- function(lfqdbase, spectab, year_min, year_max, currspec, froese, lang){
  # Getting plotting data
  plotdf <- make_plotdf(lfqdbase, spectab, year_min, year_max, currspec) |> 
    # Translate and factorize gear names as required
    _[, tipo_arte := translate_gear(tipo_arte, lang)]
  
  # Getting ylimits based on the total summed weights for this species by gear
  # ylim_max <- max(plotdf[, .(tot_wt=sum(adjWt)), by=tipo_arte]$tot_wt)
  
  # Making plot
  plotdf |> 
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      ggplot2::aes(x=long_bin, weight=adjWt), 
      fill='#CAAC7F',
      colour='black',
      alpha=0.8,
      linewidth=0.4,
      show.legend=F) +
    ggplot2::facet_wrap('tipo_arte', scales = 'fixed', ncol=1) +
    # ggplot2::scale_fill_brewer(palette = 'Dark2') +
    # Setting scale limits for clean viewing
    coord_cartesian(
      # ylim=c(0, ylim_max),
      xlim=c(
        floor(min(plotdf$long_bin))-1,
        ceiling(froese$LMspawn)+10
      )) +
    # scale_y_continuous(expand = c(0, 1)) +
    # Optimal length range for maximum yield
    # ggplot2::annotate('rect', 
    #                   xmin = froese$opt_range[1], xmax = froese$opt_range[2], 
    #                   ymin = -1, 
    #                   ymax = ylim_max+100,
    #                   fill='darkgreen',
    #                   alpha=0.2) +
    # Minimum catch length
    # geom_vline(xintercept = 23, linetype='dashed', colour='firebrick') +
    # ggplot2::annotate('text', 
    #                   # label='L-Min',
    #                   label='1.',
    #                   x=23, 
    #                   y=ylim_max,
    #                   fontface = 'bold',
    #                   family=labelFont,
    #                   hjust=-0.8,
    #                   # hjust=1.2,
    #                   size=labelSize) +
    # Length-of maturity
    geom_vline(xintercept = froese$Lmat, linetype='solid', colour='darkgreen') +
    # ggplot2::annotate(
    #   'text',
    #   label='L-Mat',
    #   x=froese$Lmat,
    #   y=ylim_max,
    #   fontface = 'bold',
    #   # family=labelFont,
    #   hjust=-0.8,
    #   # hjust=1.2,
    #   size=3
    # ) +
    # Length of mega-spawners
    geom_vline(xintercept = froese$LMspawn, linetype='dashed', colour='darkorange') +
    # ggplot2::annotate(
    #   'text',
    #   # label='3.',
    #   # label='L-Mspawn',
    #   # family=labelFont,
    #   fontface = 'bold',
    #   x=froese$LMspawn,
    #   y=ylim_max,
    #   hjust=-0.8,
    #   # hjust=1.2,
    #   size=3
    # ) +
    qtheme() +
    ggplot2::labs(
      y = ifelse(lang == 'ES','Peso capturado (kg)','Captured Weight (kg)'),
      x = ifelse(lang == 'ES','Longitud (cm)','Length (cm)')
    )
}

# Plotting catch seasonality
ss_plot_ssn <- function(lfqdbase, spectab, year_min, year_max, currspec, lang){
  # Getting plotting data
  plotdf <- make_plotdf(lfqdbase, spectab, year_min, year_max, currspec)
  # Getting the correct month reference vector
  mref_vec <- if(lang == 'ES') month.abb.esp else (month.abb)
  # Plotting
  plotdf |> 
    _[, month := lubridate::month(fecha)] |> 
    _[, month := factor(mref_vec[month], levels=mref_vec)] |> 
    ggplot2::ggplot(ggplot2::aes(x = month,y = adjWt,)) +
    # ggplot2::geom_col(fill = '#008080', alpha=0.9) +
    ggplot2::geom_col(fill = 'darkorange', alpha=0.8) +
    ggplot2::scale_x_discrete(drop = F) +
    qtheme() +
    ggplot2::labs(
      x = NULL,
      y = ifelse(lang == 'ES','Peso capturado (kg)','Catch Weight (kg)')
    )
}
  
# Plotting the CPUE per length-class against the mean CPUE
ss_plot_cpue_pl <- function(lfqdbase, spectab, year_min, year_max, currspec, lang){
  # Getting plotting data
  plotdf <- make_plotdf(lfqdbase, spectab, year_min, year_max, currspec)
  
  # Preparing dataframe for CPUE per length class in this period
  cpue_pl_tab <- plotdf |> 
    # Calculating CPUE and dates
    _[, `:=`(
      cpue = adjWt/tot_boats,
      year = lubridate::year(fecha),
      month = lubridate::month(fecha)
    )] |> 
    _[, fecha2 := lubridate::ymd(paste(year, month, '01', sep='-'))] |> 
    # Summarzing CPUE per length-class
    _[, .(
      mn_cpue = mean(cpue, na.rm=T),
      sd_cpue = sd(cpue, na.rm=T),
      n_cpue = length(!is.na(cpue)),
      period = 1
    ), by=.(long_bin)] |> 
    _[, se := sd_cpue/sqrt(n_cpue)]
  
  # Calculating the overall mean CPUE for this period
  ov_cpue <- plotdf |> 
    # Calculating CPUE and dates
    _[, `:=`(
      cpue = adjWt/tot_boats,
      year = lubridate::year(fecha),
      month = lubridate::month(fecha)
    )] |> 
    _[, .(
      mn = mean(cpue, na.rm=T),
      sd = sd(cpue, na.rm=T),
      n = length(!is.na(cpue)),
      period = 1
    )] |> 
    _[, se := sd/sqrt(n)] |> 
    _[, `:=`(
      cimin = mn - 1.96*se,
      cimax = mn + 1.96*se
    )]
  
  # Plotting CPUE per length class against mean CPUE
  merge(cpue_pl_tab,
        ov_cpue[, c('period','cimin', 'cimax'), with=F],
        by='period') |> 
    ggplot2::ggplot(ggplot2::aes(x = long_bin, y = mn_cpue)) +
    ggplot2::geom_hline(
      data=ov_cpue, 
      ggplot2::aes(yintercept = mn),
      linetype='dashed',
      colour='darkgreen'
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin=cimin, 
        ymax=cimax 
      ), 
      fill='darkgreen',
      alpha=0.2
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = (mn_cpue-1.96*se), ymax=(mn_cpue+1.96*se)), 
      width=0.2
    ) +
    ggplot2::geom_line() +
    qtheme() +
    ggplot2::labs(
      x = ifelse(lang == 'ES','Longitud (cm)','Length (cm)'),
      y = ifelse(lang == 'ES','CPUE (kg/dia)','CPUE (kg/day)')
    )
}
