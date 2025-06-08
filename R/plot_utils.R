# Author: Saeesh Mangwani
# Date: 2024-02-19

# Description: Shared utility functions for all plots

# ==== Functions ====
sysfonts::font_add_google('Inter', 'Inter')
showtext::showtext_auto()
qtheme <- function(size=24, family='Inter'){
  ggplot2::theme_minimal(size, family) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill='white', colour=NA),
      # legend.position = 'bottom',
      panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5),
      strip.text = element_text(size=rel(0.9), face='bold'),
      axis.text = element_text(size=rel(0.9))
    )
}
