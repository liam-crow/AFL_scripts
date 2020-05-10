
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% 
    select(id, first_name, surname, w_l, starts_with('umpire')) %>% 
    unite(all_umps, umpire_1, umpire_2, umpire_3, umpire_4, sep = ', ') %>% 
    filter(
        grepl('care', surname, ignore.case = T),
        grepl('care', all_umps, ignore.case = T),
    ) %>% View()
    group_by(id, first_name, surname, frees_against) %>% count()

afltables %>% 
    select(id, first_name, surname, w_l, frees_for, frees_against, starts_with('umpire')) %>% 
    unite(all_umps, umpire_1, umpire_2, umpire_3, umpire_4, sep = ', ') %>% 
    filter(
        grepl('care', surname, ignore.case = T),
        grepl('care', all_umps, ignore.case = T),
    ) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        sum_ff = sum(frees_for),
        sum_fa = sum(frees_against)
    ) 

afltables %>% 
    group_by(id, first_name, surname, w_l) %>% 
    count() %>% View()


