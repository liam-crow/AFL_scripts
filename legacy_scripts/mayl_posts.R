
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

disp <- 19
# disposals
afltables %>% 
    filter(season > 1964) %>% 
    group_by(id, first_name, surname, disposals) %>% 
    count() %>% 
    filter(disposals == disp) %>% 
    arrange(-n)

#disp and jumper no
afltables %>% 
    filter(season > 1964) %>% 
    group_by(id, first_name, surname, disposals, jumper_no) %>% 
    count() %>% ungroup() %>% 
    filter(disposals == disp, jumper_no == disp) %>% arrange(-n)


afltables %>% 
    filter(season > 1964, disposals == disp) %>% 
    dplyr::select(
        date, id, first_name, surname, jumper_no, disposals,
        kicks, handballs, goals, behinds, brownlow_votes) %>% View()


afltables %>% 
    filter(season > 1964) %>% 
    dplyr::select(
        date, id, first_name, surname, jumper_no, disposals,
        kicks, handballs, goals, behinds, brownlow_votes) %>%
    filter(
        disposals == disp, 
        jumper_no == disp
    ) %>% 
    group_by(id, first_name, surname, jumper_no) %>% 
    count() %>% View()




afltables %>% 
    filter(season > 1965) %>% 
    group_by(id, first_name, surname, disposals) %>% 
    count() %>% View()

afltables %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    summarise(
        paste(disposals, collapse = ',')
    ) %>% View()

afltables %>% 
    filter(season > 1964) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    summarise(
        most_cons = rle_calc(disposals)
    ) %>% View()
