# Author: Saeesh Mangwani
# Date: 2024-02-19

# Description: Shared utility functions for all plots

# ==== Functions ====

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

# Helper function to render a plot card with a text at the bottom and optionally
# at the top - this function is used for all timeseries plots to generate
# consistent outputs, that can also responsively show the trend and average as
# labels around the plot. It is assumed that the labels are provided as html
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