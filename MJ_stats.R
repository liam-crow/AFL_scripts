
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% 
    select(
        id, first_name, surname, goals, behinds, 
        rebounds, goal_assists, clangers
    ) %>% 
    mutate(points = goals * 6 + behinds) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        sp = sum(points),
        ap = mean(points),
        sr = sum(rebounds),
        ar = mean(rebounds),
        sa = sum(goal_assists),
        aa = mean(goal_assists),
        sc = sum(clangers),
        ac = mean(clangers)
    ) %>% View()

afltables %>%
    filter(frees_against >= 5, season >= 1986) %>% 
    group_by(id, first_name, surname) %>% 
    count() %>% arrange(-n)
