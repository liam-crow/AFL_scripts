
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

# Hey Statmins, was wondering what player has polled 
# the most Brownlow votes in games where someone 
# else with the same name is also playing. 
# E.g. games where both Josh Kennedy’s have been playing

afltables %>% 
    select(
        date, season, round, home_team, away_team, 
        id, first_name, surname, playing_for, brownlow_votes, goals, disposals, playing_for_score, opp_score
    ) %>% 
    group_by(season, round, home_team, away_team, first_name, surname) %>% 
    mutate(n = max(row_number())) %>% 
    filter(any(brownlow_votes > 0)) %>% 
    ungroup() %>% 
    filter(n > 1) %>% View()

