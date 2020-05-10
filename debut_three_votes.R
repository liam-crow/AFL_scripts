
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% 
    select(season, date, id, first_name, surname, brownlow_votes) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(game_no = row_number()) %>% 
    filter(game_no == 1, brownlow_votes == 3) %>% ungroup() %>%  
    arrange(desc(date))
    
