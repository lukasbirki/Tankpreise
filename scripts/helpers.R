library(plyr)
library(readr)
library(tidyverse)
library(sf)
library(lubridate)
library(hrbrthemes)
library(ggExtra)


theme_update(
  plot.background = element_rect(fill = "#fafaf5", color = "#fafaf5"),
  panel.background = element_rect(fill = NA, color = NA),
  panel.border = element_rect(fill = NA, color = NA),
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(size = 10),
  axis.ticks = element_blank(),
  axis.title.y = element_text(size = 13, margin = margin(r = 10)),
  legend.title = element_text(size = 9),
  plot.caption = element_text(
    family = "Special Elite",
    size = 10,
    color = "grey70",
    face = "bold",
    hjust = .5,
    margin = margin(5, 0, 20, 0)
  ),
  plot.margin = margin(10, 25, 10, 25)
)