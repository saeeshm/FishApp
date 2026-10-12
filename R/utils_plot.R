# Author: Saeesh Mangwani
# Date: 2024-02-19

# Description: Shared plotting utilities for the entire app. Defines font
# management, the generic ggplot theme, and the Coral colour palette.

# ==== Functions ====

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
