
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% 
    select(season, round, home_team, away_team) %>% distinct() %>% 
    mutate(alpha_tf = (home_team < away_team)) %>% 
    group_by(season, round) %>% 
    summarise(
        sum_alpha = sum(alpha_tf),
        count = n()
    ) %>% 
    filter(sum_alpha == count) %>% arrange(-count)

# round 3  2016
# round 15 2013

afltables %>% 
    select(season, round, playing_for, opp, w_l) %>% distinct() %>% 
    filter(w_l == "W") %>% 
    mutate(alpha_tf = (playing_for > opp)) %>% 
    group_by(season, round) %>% 
    summarise(
        sum_alpha = sum(alpha_tf),
        count = n()
    ) %>% 
    filter(sum_alpha == count) %>% arrange(-count)

# round 15 2009
# round 

View(afltables)
