# devtools::install_github("jimmyday12/fitzRoy")

library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)


afltables_all <- fitzRoy::get_afltables_stats()

names(afltables_all) <- to_snake_case(names(afltables_all))

# afl_player_jumper <- 
    afltables_all %>% select(id, first_name, surname, jumper_no) %>% distinct() %>% 
        filter(jumper_no == 19) %>% 
        unite(name, first_name, surname, sep = '') %>% 
        mutate(name = tolower(name)) %>% 
        filter(grepl('c', name)) %>% 
        filter(grepl('o', name)) %>% 
        filter(grepl('v', name)) %>% 
        filter(grepl('i', name)) %>% 
        filter(grepl('d', name)) 
