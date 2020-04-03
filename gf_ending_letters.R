# devtools::install_github("jimmyday12/fitzRoy")

library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

afltables <- fitzRoy::get_afltables_stats(start_date = "2000-01-01")

names(afltables) <- to_snake_case(names(afltables))

afl_gf <- afltables %>% select(season, round, first_name, surname, playing_for, home_team, away_team, home_score, away_score) %>% 
  filter(round == "GF",) %>% 
  mutate(
    win_loss  = case_when(
      playing_for == home_team & home_score > away_score ~ 'W',
      playing_for == away_team & home_score > away_score ~ 'L',
      playing_for == home_team & home_score < away_score ~ 'L',
      playing_for == away_team & home_score < away_score ~ 'W',
      TRUE ~ 'D'
    )
  ) %>% filter(win_loss == 'W')

n <- 10
afl_gf %>% mutate(first_lastn = to_snake_case(str_sub(first_name, -n))) %>% group_by(first_lastn) %>% count() %>% arrange(n)

View(afl_gf)
