# devtools::install_github("jimmyday12/fitzRoy")

library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)

afltables <- fitzRoy::get_afltables_stats()

names(afltables) <- to_snake_case(names(afltables))

afl_id <- afltables %>% select(id, first_name, surname) %>% distinct()

raw_game <- afltables %>% select(season, round, home_team, hq_4_g, hq_4_b, away_team, aq_4_g, aq_4_b) %>% distinct() %>% 
    mutate(round = as.numeric(round)) %>% drop_na()

game_home <- raw_game %>% select(season, round, team = home_team, goals = hq_4_g, behinds = hq_4_b)
game_away <- raw_game %>% select(season, round, team = away_team, goals = aq_4_g, behinds = aq_4_b)

all_games <- rbind(game_home, game_away)

all_games %>% group_by(season, team) %>% 
    filter(goals <= round) %>% count() %>% arrange(-n)
