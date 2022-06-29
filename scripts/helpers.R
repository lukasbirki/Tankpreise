library(plyr)
library(readr)
library(tidyverse)
library(sf)
library(lubridate)
library(hrbrthemes)
library(ggExtra)
library(gganimate)
library(gifski)
library(png)
library(ggtext)

theme_update(
  plot.background = element_rect(fill = "#fafaf5", color = "#fafaf5"),
  legend.background = element_rect(fill = "#fafaf5", color = "#fafaf5"),
  panel.background = element_rect(fill = NA, color = NA),
  panel.border = element_rect(fill = NA, color = NA),
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank(),
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

theme_set(theme_minimal(base_family = "Lato"))
library(showtext)
font_add_google("Lato")
showtext_auto()


theme_update(
  # Remove title for both x and y axes
  axis.title = element_blank(),
  # Axes labels are grey
  axis.text = element_text(color = "grey40"),
  # The size of the axes labels are different for x and y.
  axis.text.x = element_text(size = 20, margin = margin(t = 5)),
  axis.text.y = element_text(size = 17, margin = margin(r = 5)),
  # Also, the ticks have a very light grey color
  axis.ticks = element_line(color = "grey91", size = .5),
  # The length of the axis ticks is increased.
  axis.ticks.length.x = unit(1.3, "lines"),
  axis.ticks.length.y = unit(.7, "lines"),
  # Remove the grid lines that come with ggplot2 plots by default
  panel.grid = element_blank(),
  # Customize margin values (top, right, bottom, left)
  plot.margin = margin(20, 40, 20, 40),
  # Use a light grey color for the background of both the plot and the panel
  plot.background = element_rect(fill = "grey98", color = "grey98"),
  panel.background = element_rect(fill = "grey98", color = "grey98"),
  # Customize title appearence
  plot.title = element_text(
    color = "grey10", 
    size = 28, 
    face = "bold",
    margin = margin(t = 15)
  ),
  # Customize subtitle appearence
  plot.subtitle = element_markdown(
    color = "grey30", 
    size = 16,
    lineheight = 1.35,
    margin = margin(t = 15, b = 40)
  ),
  # Title and caption are going to be aligned
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.caption = element_text(
    color = "grey30", 
    size = 13,
    lineheight = 1.2, 
    hjust = 0,
    margin = margin(t = 40) # Large margin on the top of the caption.
  )
)


fill_data_gaps <- function(data, xvar, yvar, levelvar) {
  xv <- deparse(substitute(xvar))
  yv <- deparse(substitute(yvar))
  lv <- deparse(substitute(levelvar))
  
  data <- data %>% arrange({{xvar}}) # not needed?
  
  grp <- ifelse(data[[yv]] >= data[[lv]], "up", "down")
  
  sp <- split(data, cumsum(grp != lag(grp, default = "")))
  
  # calculate the intersections
  its <- lapply(seq_len(length(sp) - 1), function(i) {
    lst <- sp[[i]] %>% slice(n())
    nxt <- sp[[i + 1]] %>% slice(1)
    it <- find_intercept(x1 = lst[[xv]], x2 = nxt[[xv]],
                         y1 = lst[[yv]], y2 = nxt[[yv]],
                         l1 = lst[[lv]], l2 = nxt[[lv]])
    it[[lv]] <- it[["y"]]
    setNames(as_tibble(it), c(xv, yv, lv))
  })
  
  # insert the intersections at the correct values
  for (i in seq_len(length(sp))) {
    dir <- ifelse(mean(sp[[i]][[yv]]) > mean(sp[[i]][[lv]]), "up", "down")
    if (i > 1) sp[[i]] <- bind_rows(its[[i - 1]], sp[[i]]) # earlier interpolation
    if (i < length(sp)) sp[[i]] <- bind_rows(sp[[i]], its[[i]]) # next interpolation
    sp[[i]] <- sp[[i]] %>% mutate(.dir = dir)
  }
  # combine the values again
  bind_rows(sp)
}

s = sort(rexp(100))

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

range01(s)
