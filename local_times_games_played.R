
# devtools::install_github("jimmyday12/fitzRoy")
# install.packages("patchwork")

library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)

afltables <- fitzRoy::get_afltables_stats()

names(afltables) <- to_snake_case(names(afltables))

afl_id <- afltables %>% select(id, first_name, surname) %>% distinct()

local_time_games_played <- afltables %>% group_by(id, first_name, surname) %>% arrange(date) %>% 
    mutate(games_played = row_number()) %>% ungroup() %>% 
    filter(date > "1970-01-01") %>% 
    select(id, first_name, surname, local_start_time, games_played) %>% 
    mutate(
        local_10 = local_start_time/10,
        local_100 = local_start_time/100,
        local_pm = local_start_time-1200,
        local_10_pm = (local_start_time-1200)/10,
        local_100_pm = (local_start_time-1200)/100
    ) %>% 
    filter(
        games_played == local_10 |
        games_played == local_100 |
        games_played == local_pm |
        games_played == local_10_pm |
        games_played == local_100_pm
    ) %>% arrange(desc(games_played))

local_time_games_played %>% group_by(id, first_name, surname) %>% count() %>% arrange(-n)

write.csv(local_time_games_played, "local_times_games_played.csv")

View(local_time_games_played)
