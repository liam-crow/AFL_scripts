
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% 
    select(season, round, date, home_team, away_team, home_score, away_score, w_l) %>% distinct() %>% 
    mutate(
        day = weekdays(date)
    ) %>% 
    filter(w_l == 'D') %>% 
    arrange(date) %>% 
    group_by(day) %>% 
    count()

afltables %>% 
    select(season, round, date, venue, home_team, away_team, home_score, away_score) %>% distinct() %>% 
    filter(venue == 'Kardinia Park', away_team == 'Hawthorn') %>% 
    arrange(desc(date)) %>% 
    mutate(ratio = home_score/away_score) %>% View()

afltables %>% 
    filter(season >= 2000) %>% 
    group_by(id, first_name, surname, w_l) %>% 
    count() %>% ungroup() %>% 
    pivot_wider(
        names_from = w_l,
        values_from = n,
        values_fill = list(n = 0)
    ) %>% 
    mutate(
        win_ratio = W/(W+L+D),
        total_games = W+L+D
    ) %>% View()
    
    
