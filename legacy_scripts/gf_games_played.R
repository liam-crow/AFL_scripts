
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

gf_games <- afltables %>% 
    select(season, round, date, id, first_name, surname, w_l) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(total_games = row_number()) %>% ungroup() %>% 
    filter(season >= 1965, round == 'GF', w_l !='D')

library(ggplot2)

ggplot(gf_games, aes(x = season, y = total_games, colour = w_l)) + 
    geom_point() + geom_smooth(method = 'gam')

gf_games %>% arrange(-total_games)

