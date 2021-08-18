
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")


afltables %>% 
    filter(season >= 2005) %>% 
    select(date, id, first_name, surname, marks_inside_50, marks) %>% 
    filter(
        first_name == 'Mark',
        month(date) == 3
    ) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        s_m50 = sum(marks_inside_50),
        s_m = sum(marks)
    ) %>% View()

afltables %>% 
    filter(weekdays(date) == 'Friday', season > 1965) %>% 
    select(id, first_name, surname, goals, behinds) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        s_g = sum(goals),
        s_b = sum(behinds)
    ) %>% ungroup() %>% 
    mutate(acc = s_g/(s_g + s_b)*100, total_shots = s_g + s_b) %>% 
    View()
