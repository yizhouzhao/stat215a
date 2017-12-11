library(corrplot)
library(ggplot2)
library(reshape2)
library(Matrix)
library(dplyr)
library(scatterplot3d) 
library(graphics)
library(spatstat)
library(gridExtra)

#Set up working directory
#setwd("E:/courses/Stat215A/lab_final/")
#getwd()


grid_arrange_shared_legend <- function(...,
                                       ncol = length(list(...)),
                                       nrow = 1,
                                       position = c("bottom", "right")) {
  # Function to share a legend between multiple plots
  # using grid.arrange. Taken from:
  # https://github.com/tidyverse/ggplot2/wiki/
  # share-a-legend-between-two-ggplot2-graphs
  library(grid)
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + 
                    theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x +
                 theme(legend.position = "none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl), 
                                            legend,ncol = 1,
                                            heights = unit.c(unit(1, "npc")
                                                             - lheight,
                                                             lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend, ncol = 2,
                                           widths = unit.c(unit(1, "npc") -
                                                             lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
}
