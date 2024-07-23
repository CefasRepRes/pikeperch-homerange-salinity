# libraries & functions ---------------------------------------------------

# libraries ---------------------------------------------------------------

library(ggplot2) # needed here for theme
library(patchwork) # for combining ggplots
library(extrafont) # for using Times New Roman
library(knitr) # needed?
library(data.table) # for reading tables

# functions ---------------------------------------------------------------

# ggplot style
sgg <- function(fs = 16) {
  theme(text = element_text(family = "Times New Roman", size = fs),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text.x = element_text(size = fs),
        axis.text = element_text(size = fs),
        axis.title = element_text(size = fs),
        legend.title = element_text(size = fs),
        legend.text = element_text(size = fs),
        complete = FALSE)
}

# remove space between panels
squeeze_panels <- theme(panel.spacing = grid::unit(0, "lines")) 

