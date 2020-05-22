
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)
library(fitzRoy)

source("load_afltables.R")

afltables %>% 
    select(season, round, home_team, away_team, playing_for, id, first_name, surname) %>% 
    group_by(season, round, home_team, away_team, playing_for, first_name) %>% 
    count() %>% View()
