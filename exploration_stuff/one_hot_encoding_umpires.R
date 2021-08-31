library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
source("load_afltables.R")
source("fryzigg_data.R")

View(fryzigg_data)
afltables %>% 
    select(season, round, date, home_team, away_team, frees_for, frees_against, umpire_1, umpire_2, umpire_3, umpire_4) %>% 
    filter(date >= '2016-06-19') %>% 
    group_by(season, round, date, home_team, away_team, umpire_1, umpire_2, umpire_3, umpire_4) %>% 
    summarise(
        s_ff = sum(frees_for),
        s_fa = sum(frees_against),
    ) %>% filter(s_ff != s_fa) %>% View()
