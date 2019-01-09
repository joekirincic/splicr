
#library(tidyverse)
#library(lubridate)
#library(rlang)

plot_errors <- function(df, x, y, threshold){
  
  x_en <- x
  y_en <- y
  
  # Ignoring `x_lower` and `x_upper` until we can figure out how to generalize the bounds on the x-axis.
  #x_lower <- df %>% `$`(!!x_en) %>% min(.)
  #x_upper <- df %>% `$`(!!y_en) %>% max(.)
  
  prop_outside <- df %>% dplyr::select(!!y_en) %>% dplyr::summarise(prop = mean(dplyr::if_else(!!y_en > threshold, 1, 0))) %>% signif(., digits = 3) *100
  count_outside <- df %>% dplyr::select(!!y_en) %>% dplyr::summarise(count = sum(dplyr::if_else(. > threshold, 1, 0)))
  
  ggplot2::ggplot(data = df, ggplot2::aes(x = !!x_en, y = !!y_en)) +
    ggplot2::geom_jitter(color = "black", width = 0.1, height = 0.0) +
    ggplot2::geom_hline(yintercept = threshold, color = "green") +
    ggplot2::geom_hline(yintercept = -1*threshold, color = "green") +
    ggplot2::geom_rect(xmin = 0, xmax = 4, ymin = -1*threshold, ymax = threshold, fill = "yellowgreen", alpha = 0.01) +
    ggplot2::ggtitle(glue::glue("{prop_outside}% of the data ({count_outside} observations) lie outside the chosen threshold")) +
    ggplot2::theme_minimal()
}