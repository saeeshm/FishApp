# Author: Saeesh Mangwani
# Date: 2024-02-19

# Description: Shared utility functions for all plots

# ==== Functions ====
qtheme <- function(size=12, family='serif'){
  ggplot2::theme_minimal(size, family) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill='white', colour=NA),
      # legend.position = 'bottom',
      panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5))
}
