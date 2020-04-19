
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% 
    filter(year(date) > 1965, month(date) == 4) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        recent_game = max(date),
        s_kicks = sum(kicks),
        s_handb = sum(handballs),
    ) %>% View()
    
