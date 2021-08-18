
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% 
    select(date, season, round, id, first_name, surname, goals) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(goals_sum = cumsum(goals)) %>% 
    filter(weekdays(date) == 'Thursday', goals_sum > 600, goals > 1)

afltables %>% 
    select(date, season, round, id, first_name, surname, goals, behinds) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(
        goals_sum = cumsum(goals),
        behinds_sum = cumsum(behinds),
        points_sum = cumsum(goals*6 + behinds),
        game_sum = row_number()
    ) %>% 
    filter(game_sum == goals_sum) %>% arrange(-game_sum)
