
library(tidyverse)
library(lubridate)
library(rlang)

plot_errors <- function(df, x, y, threshold){
  
  x_en <- x
  y_en <- y
  
  # Ignoring `x_lower` and `x_upper` until we can figure out how to generalize the bounds on the x-axis.
  #x_lower <- df %>% `$`(!!x_en) %>% min(.)
  #x_upper <- df %>% `$`(!!y_en) %>% max(.)
  
  #threshold_en <- enquo(threshold)
  
  ggplot(data = df, aes(x = !!x_en, y = !!y_en)) +
    geom_jitter(color = "black", width = 0.1, height = 0.0) +
    geom_hline(yintercept = threshold, color = "green") +
    geom_hline(yintercept = -1*threshold, color = "green") +
    geom_rect(xmin = 0, xmax = 4, ymin = -1*threshold, ymax = threshold, fill = "yellowgreen", alpha = 0.01) +
    ggtitle("Differences per entity", subtitle = glue::glue("Tolerance threshold = | {threshold} |")) +
    theme_minimal()
}