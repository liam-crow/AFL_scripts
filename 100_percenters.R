
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        debut = min(date),
        sum_op = sum(one_percenters),
        pf = paste(unique(playing_for), collapse = ', ')
    ) %>% View()
