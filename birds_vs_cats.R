
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% 
    select(season, round, playing_for, w_l) %>% distinct() %>% 
    filter(
        playing_for == "Richmond" & w_l %in% c('L') |
        playing_for == "West Coast" & w_l %in% c('L') |
        playing_for == "North Melbourne" & w_l %in% c('L') |
        playing_for == "Geelong" & w_l %in% c('L') |
        playing_for == "Carlton" & w_l %in% c('W')
    ) %>% 
    group_by(season, round) %>% count() %>% arrange(-n, -season)

afltables %>% 
    select(season, round, playing_for, w_l) %>% distinct() %>% 
    filter(
        playing_for == "Hawthorn" & w_l %in% c('W','D') |
        playing_for == "Sydney" & w_l %in% c('W','D') |
        playing_for == "Adelaide" & w_l %in% c('W','D') |
        playing_for == "Collingwood" & w_l %in% c('W','D') |
        playing_for == "West Coast" & w_l %in% c('W','D') |
        playing_for == "Geelong" & w_l %in% c('L','D') |
        playing_for == "Brisbane Lions" & w_l %in% c('L','D') |
        playing_for == "Richmond" & w_l %in% c('L','D')
    ) %>% 
    group_by(season, round) %>% count() %>% arrange(-n, -season)

