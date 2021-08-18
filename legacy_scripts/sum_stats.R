# devtools::install_github("jimmyday12/fitzRoy")

library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)

afltables <- fitzRoy::get_afltables_stats()

names(afltables) <- to_snake_case(names(afltables))

agg_stats <- afltables %>% group_by(id, first_name, surname) %>% 
    summarise(
        deb   = min(date),
        games = n(),
        t_hb  = sum(handballs),
        t_ki  = sum(kicks),
        t_ma  = sum(marks),
    ) %>% 
    filter(
        deb > "1970-01-01",
        games >= 50
    ) %>% arrange(t_hb)

write.csv(agg_stats, "agg_sum_hb_ki_ma.csv")

agg_stats_games <- 
afltables %>% group_by(id, first_name, surname, handballs) %>% 
    summarise(
        deb   = min(date),
        games = n()
    ) %>% 
    filter(
        deb > "1970-01-01",
        handballs < 6
    ) %>% arrange(-games)

write.csv(agg_stats_games, "agg_sum_games_hb.csv")
