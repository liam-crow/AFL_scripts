
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% 
    filter(season >= 1965) %>%
    select(season, date, round, id, first_name, surname, goals, behinds) %>% 
    mutate(
        day = day(date),
        mon = month(date),
        yr  = year(date)
    ) %>% 
    filter(
        goals == day,
        behinds == mon
    ) %>% View()
