# devtools::install_github("jimmyday12/fitzRoy")
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- get_afltables_stats()
names(afltables) <- names(afltables) %>% snakecase::to_snake_case()

afltables %>% 
    # filter(!(round %in% c('EF','QF','PF','SF','GF'))) %>%
    group_by(id, first_name, surname) %>% 
    mutate(games = max(row_number())) %>% 
    select(id, first_name, surname, date, season, round, playing_for, home_team, away_team, games) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(desc(date)) %>% 
    filter(row_number() %in% 1:3, games >= 3) %>% #, max(row_number()) == 2) %>% 
    mutate(against = if_else(playing_for == home_team, away_team, home_team)) %>% 
    mutate(no_uniq = length(unique(against))) %>% #View()
    filter(no_uniq == 1) %>% 
    select(id, first_name, surname) %>% distinct()
    View()
