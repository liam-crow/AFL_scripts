
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

# 1	30 Andrew McLeod
# 2	1114 Jordan	McMahon

afltables %>% 
    filter(season > 1964) %>% 
    select(season, date, id, first_name, surname, disposals, goals) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(
        diff_from_prev_disp = disposals - lag(disposals),
        diff_from_prev_goal = goals - lag(goals),
        diff_disp_tf = if_else(diff_from_prev_disp >= 0, T, F),
        # diff_goal_tf = if_else(diff_from_prev_goal > 0, T, F)
    ) %>% View()
    group_by(id, first_name, surname) %>% 
    summarise(
        cons_len = rle_len_calc(diff_disp_tf),
        cons_val = rle_val_calc(diff_disp_tf)
    ) %>% View()

rle_calc(c(T,F,T,T,T,F,T,T))
rle_len_calc(c(T,F,T,T,T,F,T,T))
rle_val_calc(c(T,F,T,T,T,F,T,T))
