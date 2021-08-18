
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)
library(fitzRoy)

source("load_afltables.R")

afltables %>% 
    select(date, id, first_name, surname, playing_for, goals, kicks) %>% 
    group_by(id, first_name, surname, playing_for) %>% 
    arrange(date) %>% 
    mutate(games_club = row_number()) %>% ungroup() %>%
    filter(games_club == 1, goals == 1, kicks == 1) %>% 
    group_by(id, first_name, surname) %>% count() %>% View()
    
