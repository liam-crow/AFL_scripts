# devtools::install_github("jimmyday12/fitzRoy")

library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

afltables <- fitzRoy::get_afltables_stats()

names(afltables) <- to_snake_case(names(afltables))

afltables_gf <- afltables %>% 
  filter(round == "GF",) %>% 
  mutate(
    win_loss  = case_when(
      playing_for == home_team & home_score > away_score ~ 'W',
      playing_for == away_team & home_score > away_score ~ 'L',
      playing_for == home_team & home_score < away_score ~ 'L',
      playing_for == away_team & home_score < away_score ~ 'W',
      TRUE ~ 'D'
    )
  ) %>% filter(win_loss == 'L') %>% 
  select(season, round, date, id, first_name, surname)

afltables_gf %>% group_by(id, first_name, surname) %>% 
  summarise(
    games = n(),
    years = paste(season, collapse = ' '),
    myear = max(season),
    diff  = max(season) - min(season)
  ) %>% 
  filter(
    games > 2,
    diff  > 10
  ) %>% View()

