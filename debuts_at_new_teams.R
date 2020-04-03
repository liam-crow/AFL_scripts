# devtools::install_github("jimmyday12/fitzRoy")

library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

afltables <- fitzRoy::get_afltables_stats()

names(afltables) <- to_snake_case(names(afltables))

afltables %>% select(date, local_start_time, playing_for, id, first_name, surname) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(
        game_num = row_number(),
        playing_for = trimws(playing_for)
    ) %>% 
    ungroup() %>% 
    filter(game_num <= 150) %>% 
    group_by(id, first_name, surname) %>% 
    filter(length(unique(playing_for)) > 2) %>% 
    group_by(playing_for, id, first_name, surname) %>%
    filter(date == min(date) | game_num == 150) %>%
    group_by(id, first_name, surname) %>% 
    filter(any(game_num == 150)) %>% View()

data <- afltables %>% select(date, local_start_time, playing_for, id, first_name, surname) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(
        game_num = row_number(),
        playing_for = trimws(playing_for)
    ) %>% 
    group_by(playing_for, id, first_name, surname) %>% 
    filter(date == min(date)) %>% 
    group_by(id, first_name, surname) %>% 
    mutate(
        team_num = row_number()
    ) %>% 
    ungroup() %>% filter(game_num != 1, team_num == 5)

lattice::densityplot(data$game_num)

mean(data$game_num)
