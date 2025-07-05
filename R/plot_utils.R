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

coral_colours = rep(c("#3ebbed","#f69a94","#eddfca","#90d6e5",
    "#00afa4","#0d659a","#a8426e"),2)

coral_colours_fill = ggplot2::scale_fill_manual(
  values = coral_colours
)

coral_colours_outline = ggplot2::scale_color_manual(
  values = coral_colours
)

# Additional options
#ffb48f (peachy coral)
#d98677 (muted clay rose)
#c2e1dc (soft minty teal)