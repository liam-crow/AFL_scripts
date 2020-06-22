
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% 
    # filter(season >= 1965) %>% 
    select(season, round, date, playing_for, id, first_name, surname, w_l) %>% 
    filter(w_l == "W") %>% 
    group_by(id, first_name, surname) %>% 
    mutate(total_clubs = length(unique(playing_for))) %>% 
    filter(total_clubs == 1) %>% 
    arrange(date) %>% 
    mutate(diff = date - lag(date)) %>% #View()
    arrange(-diff) %>% View()

afltables %>% 
    select(season, round, date, id, first_name, surname, w_l) %>% 
    filter(round == 'GF', w_l == "W") %>% 
    arrange(date) %>% 
    group_by(id, first_name, surname) %>% 
    mutate(diff = date - lag(date)) %>% 
    arrange(-diff) %>% View()
