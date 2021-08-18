
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

diff_date_data <- afltables %>% 
    select(date, season, round, playing_for, w_l) %>% distinct() %>% 
    filter(season > 1905) %>% 
    arrange(date) %>% 
    group_by(playing_for) %>% 
    mutate(day_diff = as.numeric(date - lag(date), units = 'days')) %>% 
    filter(day_diff > 80, round != 1)

diff_date_data %>% 
    group_by(playing_for, w_l) %>% 
    count() %>% 
    pivot_wider(
        id_cols = playing_for,
        names_from = w_l,
        values_from = n
    ) %>% ungroup() %>% 
    mutate(w_ratio = W/(W+L)) %>% 
    select(playing_for, W, L, w_ratio) %>% 
    arrange(-w_ratio) %>% View()

class(diff_date_data$day_diff)
