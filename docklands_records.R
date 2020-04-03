# devtools::install_github("jimmyday12/fitzRoy")

library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)

afltables <- fitzRoy::get_afltables_stats()

names(afltables) <- to_snake_case(names(afltables))

docklands_afltables <- afltables %>% 
    mutate(venue =  trimws(venue)) %>% 
    filter(venue == 'Docklands')

docklands_afltables %>% group_by(id, first_name, surname) %>% 
    summarise(
        sum_k = sum(kicks),
        sum_h = sum(handballs),
        sum_g = sum(goals),
        sum_b = sum(behinds),
        sum_ff= sum(frees_for),
        sum_fa= sum(frees_against),
        sum_bo= sum(bounces),
        sum_m = sum(marks),
        games = n()
    ) %>% View()
