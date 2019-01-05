
library(tidyverse)
library(lubridate)
library(tesseract)
source("utilities.R")
source("app.R")

eng <- tesseract("eng")
text <- tesseract::ocr(
  "C:\\Users\\JKirincic\\OneDrive - OnShift\\R Projects\\OnShift Analytics\\testing\\sample_table_image.png",
  engine = eng
)

labor_budget_analysis_check <- function(text){
  result <- read_delim(text, delim = " ", col_names = FALSE) %>% `[`(1:(nrow(.)-2),) %>%
    mutate(date = as.Date(paste0(X1, " ", X2), "%B %d"),
           census = as.integer(X3),
           actual_hppd = as.numeric(X4),
           target_hppd = as.numeric(X5),
           assigned_hours = as.numeric(X6),
           target_hours = as.numeric(X7),
           required_hours = as.numeric(X11)
    ) %>%
    select_if(., .p = !str_detect(names(.), "X"))
  return(result)
}