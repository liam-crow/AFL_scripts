
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)
library(fitzRoy)

source("load_afltables.R")

afltables %>% 
    group_by(id, first_name, surname) %>% 
    mutate(
        total_goals = sum(goals),
        total_games = n(),
        debut = min(date)
    ) %>% ungroup() %>% 
    filter(goals >= 10, total_goals <= 100) %>% 
    select(id, first_name, surname, playing_for, date, goals, total_goals, total_games) %>% 
    arrange(desc(date))
