library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
source("load_afltables.R")
source("load_fryzigg.R")

fryzigg_data %>% 
    select(season, date, round, id, first_name, surname, fantasy_points, afl_fantasy_score) %>% 
    filter(fantasy_points != afl_fantasy_score)

afltables %>% 
    select(season, round, date, playing_for, opp, playing_for_score, opp_score) %>% 
    distinct() %>% 
    filter(playing_for_score == 2*opp_score) %>% 
    View()
