
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% 
    filter(month(date) == 9) %>% 
    select(season,round,date, playing_for, w_l) %>% distinct() %>% 
    mutate(playing_for = gsub('Brisbane Bears','Brisbane Lions', playing_for)) %>% 
    group_by(playing_for, w_l) %>% 
    count() %>% 
    pivot_wider(
        names_from = w_l,
        values_from = n,
        values_fill = list(n = 0)
    ) %>% ungroup() %>% 
    mutate(ratio = W/(W+L+D)*100) %>% 
    arrange(-ratio)
    
