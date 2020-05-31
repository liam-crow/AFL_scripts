
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% 
    select(season, round, home_team, away_team) %>% distinct() %>% 
    mutate(alpha_tf = (home_team > away_team)) %>% 
    group_by(season, round) %>% 
    summarise(
        sum_alpha = sum(alpha_tf),
        count = n()
    ) %>% 
    filter(sum_alpha == count) %>% arrange(-count)
