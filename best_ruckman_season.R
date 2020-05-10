
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% 
    filter(season > 66) %>% 
    select(season, id, first_name, surname, hit_outs, goals, contested_marks) %>% 
    group_by(season, id, first_name, surname) %>% 
    summarise(
        s_ho = sum(hit_outs),
        s_g = sum(goals),
        s_cm = sum(contested_marks)
    ) %>% 
    filter(s_ho > 100) %>% 
    View()
