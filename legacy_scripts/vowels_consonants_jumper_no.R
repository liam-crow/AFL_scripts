
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% 
    select(playing_for, id, first_name, surname, jumper_no) %>% distinct() %>% 
    filter(jumper_no > 0) %>% 
    mutate(
        first_v = gsub('[bcdfghjklmnpqrstvwxys-]','', first_name, ignore.case = T),
        last_v = gsub('[bcdfghjklmnpqrstvwxys-]','', surname, ignore.case = T),
        count_v = nchar(first_v) + nchar(last_v)
    ) %>%
    filter(jumper_no == count_v) %>% 
    arrange(-count_v) %>% View()

afltables %>% 
    select(playing_for, id, first_name, surname, jumper_no) %>% distinct() %>% 
    filter(jumper_no > 0) %>% 
    mutate(
        first_v = gsub('[aeiou-]','', first_name, ignore.case = T),
        last_v = gsub('[aeiou-]','', surname, ignore.case = T),
        count_v = nchar(first_v) + nchar(last_v)
    ) %>% 
    filter(jumper_no == count_v) %>% 
    arrange(-count_v) %>% View()
