
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% 
    select(season, round, playing_for, id, first_name, surname, w_l) %>% 
    group_by(id, first_name, surname) %>% 
    mutate(
        first_s = min(season),
        last_s  = max(season)
    ) %>% ungroup() %>% 
    filter(round == 'GF', w_l == "W", season == first_s | season == last_s) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        first_s = min(first_s),
        last_s = max(last_s),
        teams = paste(playing_for, collapse = ', '),
        n = n()
    ) %>% View()
    
