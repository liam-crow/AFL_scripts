
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- fitzRoy::get_afltables_stats(start_date = "1900-01-01")
names(afltables) <- snakecase::to_snake_case(names(afltables))
 
cust <- afltables %>% group_by(id, first_name, surname) %>% 
    filter((date == max(date) | date == min(date)) & max(date) != min(date)) %>% 
    mutate(margin = home_score - away_score) %>% 
    summarise(
        debut = min(date),
        last_game = max(date),
        games = n(),
        playing_for = paste0(playing_for, collapse = ', '),
        home = paste0(home_team, collapse = ', '),
        away = paste0(away_team, collapse = ', '),
        loc = paste0(venue, collapse = ', '),
        margin = paste0(margin, collapse = ', ')
    ) %>% ungroup() %>% 
    filter(month(debut) == month(last_game) & day(debut) == day(last_game) & games > 1) %>% 
    arrange(-games)

cust %>% 
    separate(playing_for, c('playing_for_1', 'playing_for_2'), sep = ', ') %>% 
    separate(home, c('home_1', 'home_2'), sep = ', ') %>% 
    separate(away, c('away_1', 'away_2'), sep = ', ') %>% 
    separate(margin, c('margin_1', 'margin_2'), sep = ', ') %>% 
    mutate(
        against_1 = if_else(playing_for_1 != home_1, home_1, away_1),
        against_2 = if_else(playing_for_2 != home_2, home_2, away_2)
    ) %>% select(-home_1, -home_2, -away_1, -away_2) %>% 
    # filter(against_1 == against_2) %>% 
    View()

