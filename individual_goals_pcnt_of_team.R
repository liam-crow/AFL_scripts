
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

# beginning of time
afltables %>% 
    select(season, date, playing_for, id, first_name, surname, goals, pq_4_g) %>% 
    group_by(playing_for, id, first_name, surname) %>% 
    summarise(
        s_goals = sum(goals),
        s_team_goals = sum(pq_4_g),
        games = n(),
        teams = paste0(unique(playing_for), collapse = ', ')
    ) %>% 
    filter(games > 40) %>% 
    mutate(pcnt = s_goals/s_team_goals*100) %>% View()
    
# debuted since 65

