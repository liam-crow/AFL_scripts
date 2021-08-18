
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% 
    filter(season > 1965) %>% 
    select(season, id, first_name, surname, goals, behinds, kicks, handballs) %>% 
    group_by(id, first_name, surname) %>% 
    mutate(debut_season = max(season)) %>% 
    group_by(season, id, first_name, surname, debut_season) %>% 
    summarise(
        sg = sum(goals),
        sb = sum(behinds),
        sk = sum(kicks),
        sh = sum(handballs),
        games = n()
    ) %>% ungroup() %>% 
    filter(
        sg != 0, sk != 0,
        sg == sb,
        sk == sh,
        season == debut_season
    )



