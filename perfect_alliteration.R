# devtools::install_github("jimmyday12/fitzRoy")

library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

afltables <- fitzRoy::get_afltables_stats()

player_id <- afltables %>% select(id, first_name, surname) %>% distinct()

first_last_char <- player_id %>% 
    filter(
        nchar(first_name) == nchar(surname),
        substr(first_name, 1, 1) == substr(surname, 1, 1)
    ) %>% 
    mutate(n = nchar(first_name)) %>% 
    arrange(-n)

first_last_char %>% group_by(n) %>% count()
