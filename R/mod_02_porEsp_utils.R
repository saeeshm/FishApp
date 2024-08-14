# Author: Saeesh Mangwani
# Date: 2024-08-09

# Description: Utility functions for the single-species indicator page
# (essentially all the functions to generate the required plot based on reactive
# inputs)

#' @import data.table
#' @import ggplot2

# ==== Functions ====

# Creating a plotting dataset by filtering for the provided species
make_plotdf <- function(dbase, year_min, year_max, currspec){
  # Getting the scientific name associated with this common name
  ncien <- spectab[nombre_comun_cln %chin% stringr::str_to_lower(currspec)]$nombre_cientifico
  # Filtering the database for data in this year period for this species
  dbase[(year(fecha) >= year_min) & 
          (year(fecha) < year_max) & 
          (nombre_cientifico %chin% ncien), ]
}

# Calculates all relevant froese indicators given an estimate of Linf, using
# empirical equations following Froese 2004 and Frose and Binholan 2000
calc_froese_indics <- function(Linf, Lmax=NULL){
  # Length of maturity
  Lmat <- exp((0.8979 * log(Linf)) - 0.0782)
  # Optimal length range 
  Lopt <- exp((1.0421 * log(Linf)) - 0.2742)
  opt_range <- c(Lopt - Lopt*0.1, Lopt + Lopt*0.1)
  # Megaspawner length
  LMspawn <- Lopt+Lopt*0.1
  return(list(
    'Lmat' = Lmat,
    'opt_range' = opt_range,
    'LMspawn' = LMspawn
  ))
}

# Plotting length-frequencies by gear for this species
ss_lfq_plot <- function(lfqdbase, year_min, year_max, currspec){
  # Getting plotting data
  plotdf <- make_plotdf(lfqdbase, year_min, year_max, currspec)
  # Making plot
  plotdf |> 
    ggplot2::ggplot() +
    ggplot2::geom_bar(
      ggplot2::aes(x=long_bin, weight=adjWt), 
      fill='white',
      colour='black',
      alpha=0.8,
      linewidth=0.4,
      show.legend=F) +
    ggplot2::scale_fill_brewer(palette = 'Dark2') +
    # Setting scale limits for clean viewing
    # coord_cartesian(ylim=c(0, ylim_max+50), 
    #                 xlim=c(
    #                   floor(min(plotdf$long_bin))-1, 
    #                   ceiling(froese$LMspawn)+10
    #                 )) +
    # scale_y_continuous(expand = c(0, 1)) +
    # Optimal length range for maximum yield
    # ggplot2::annotate('rect', 
    #                   xmin = froese$opt_range[1], xmax = froese$opt_range[2], 
    #                   ymin = -1, 
    #                   ymax = ylim_max+100,
    #                   fill='darkgreen',
    #                   alpha=0.2) +
    # Minimum catch length
    # geom_vline(xintercept = 23, 
    #            linetype='dashed', colour='firebrick') +
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
    # geom_vline(xintercept = froese$Lmat, linetype='dashed') +
    # ggplot2::annotate('text', 
    #                   label='2.',
    #                   x=froese$Lmat, 
    #                   y=ylim_max,
    #                   fontface = 'bold',
    #                   family=labelFont,
    #                   hjust=-0.8,
    #                   # hjust=1.2,
    #                   size=labelSize) +
    # Length of mega-spawners
    # geom_vline(xintercept = froese$LMspawn, 
    #            linetype='dashed', colour='darkorange') +
    # ggplot2::annotate('text', 
    #                   label='3.',
    #                   # label='L-Mspawn',
    #                   family=labelFont,
    #                   fontface = 'bold',
    #                   x=froese$LMspawn, 
    #                   y=ylim_max,
    #                   hjust=-0.8,
    #                   # hjust=1.2,
    #                   size=labelSize) +
    qtheme() +
    ggplot2::facet_wrap('tipo_arte', scales = 'fixed', drop=F, ncol=1) +
    ggplot2::labs(
      y = 'Peso capturado (kg)',
      x = 'Longitud (cm)'
    )
}

# Plotting the CPUE for this species
ss_cpue_plot <- function(lfqdbase, year_min, year_max, currspec, bygear){
  # Getting plotting data
  plotdf <- make_plotdf(lfqdbase, year_min, year_max, currspec)
  # Preparing CPUE dataframe for summarization
  cpueDat <- plotdf |> 
    _[, `:=`(
      cpue = adjWt/tot_boats,
      year = lubridate::year(fecha),
      month = lubridate::month(fecha)
    )] |> 
    _[, fecha2 := lubridate::ymd(paste(year, month, '01', sep='-'))]
  
  # If plotting by gear, summarizing by gear and month before plotting
  if(bygear){
    cpueDat |> 
      _[, .(cpue = mean(cpue, na.rm=T)), by=.(fecha2, tipo_arte)] |> 
      ggplot2::ggplot(ggplot2::aes(x = fecha2, y=cpue)) +
      ggplot2::geom_point(alpha=0.4, show.legend = F) +
      ggplot2::geom_line(alpha=0.8, show.legend = F) +
      # stat_poly_line() +
      # stat_poly_eq(use_label(c("P", "R2"))) +
      ggplot2::scale_y_continuous(limits = c(0, 3)) +
      ggplot2::facet_wrap('tipo_arte', ncol=1) +
      qtheme() +
      ggplot2::labs(
        x = NULL,
        y = 'CPUE (kg/dia)'
      )
  }else{
    cpueDat |> 
      _[, .(cpue = mean(cpue, na.rm=T)), by=.(fecha2)] |> 
      ggplot2:: ggplot(ggplot2::aes(x = fecha2, y=cpue)) +
      ggplot2::geom_point(alpha=0.4, show.legend = F) +
      ggplot2::geom_line(alpha=0.8, show.legend = F) +
      ggplot2::scale_y_continuous(limits = c(0, 5)) +
      ggplot2::geom_smooth(method='lm', colour='firebrick', linewidth=0.6) +
      # ggpmisc::stat_poly_eq(use_label(c("P", "R2"))) +
      qtheme() +
      ggplot2::labs(
        x = NULL,
        y = 'CPUE (kg/dia)'
      )
  }
}
  
# Plotting the CPUE per length-class against the mean CPUE
ss_plot_cpue_pl <- function(lfqdbase, year_min, year_max, currspec){
  # Getting plotting data
  plotdf <- make_plotdf(lfqdbase, year_min, year_max, currspec)
  
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
      x = 'Longitud (cm)',
      y = 'CPUE (kg/dia)'
    )
}

# Plotting catch seasonality
ss_plot_ssn <- function(lfqdbase, year_min, year_max, currspec){
  # Getting plotting data
  plotdf <- make_plotdf(lfqdbase, year_min, year_max, currspec)
  # Plotting
  plotdf |> 
    _[, month := lubridate::month(fecha)] |> 
    _[, month := factor(month.abb[month], levels=month.abb)] |> 
    ggplot2::ggplot(ggplot2::aes(x = month,y = adjWt,)) +
    # ggplot2::geom_col(fill = '#008080', alpha=0.9) +
    ggplot2::geom_col(fill = 'darkorange', alpha=0.8) +
    qtheme() +
    ggplot2::labs(
      x = NULL,
      y = 'Peso capturado (kg)'
    )
}
