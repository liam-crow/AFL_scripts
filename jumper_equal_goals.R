
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- fitzRoy::get_afltables_stats(start_date = "1900-01-01")
names(afltables) <- snakecase::to_snake_case(names(afltables))

afltables %>% select(id, first_name, surname, playing_for, jumper_no, goals) %>% 
    filter(jumper_no == goals, jumper_no != 0) %>% arrange(-goals) %>% View()
    group_by(id, first_name, surname) %>% 
    summarise(
        n  = n(),
        no = paste0(unique(jumper_no), collapse = ', ')
    ) %>% arrange(-n)
