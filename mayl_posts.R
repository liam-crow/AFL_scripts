
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% 
    filter(season > 1964) %>% 
    group_by(id, first_name, surname, disposals) %>% 
    count() %>% 
    filter(disposals == 10) %>% arrange(-n)

afltables %>% 
    filter(season > 1964) %>% 
    dplyr::select(
        date, id, first_name, surname, jumper_no, disposals,
        kicks, handballs, goals, behinds, brownlow_votes) %>% View()

afltables %>% 
    filter(season > 1965) %>% 
    group_by(id, first_name, surname, disposals) %>% 
    count() %>% View()
