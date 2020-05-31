
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)
library(fitzRoy)

source("load_afltables.R")

afltables %>% 
    filter(season > 1965) %>% 
    select(season, date, round, playing_for, id, first_name, surname, w_l) %>% 
    group_by(w_l, season, date, round, playing_for) %>% arrange(id) %>% 
    summarise(
        ids_ordered = paste(id, first_name, surname, collapse = ', ')
    ) %>% 
    group_by(w_l, playing_for, ids_ordered) %>% count() %>% ungroup() %>% arrange(-n) %>% 
    pivot_wider(
        names_from = w_l,
        values_from = n,
        values_fill = list(n = 0)
    ) %>% 
    mutate(
        w_ratio = W/(W+L+D)
    ) %>% View()
