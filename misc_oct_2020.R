
library(dplyr)
library(tidyr)
source("load_afltables.R")
source("fryzigg_data.R")

# 1 point finals losses ####

afltables %>% 
    select(season, round, date, playing_for, playing_for_score, opp, opp_score) %>% distinct() %>% 
    filter(!(round %in% 1:30), season > 2000) %>% 
    mutate(margin = playing_for_score - opp_score) %>% 
    filter(between(margin,-1,-1)) %>% 
    group_by(playing_for, opp) %>% 
    summarise(
        n = n(),
        seasons = paste(season, round, playing_for_score, opp_score, collapse = ', ')
    ) %>% View()

afltables %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        max_no = max(jumper_no),
        min_no = min(jumper_no),
        diff = max_no-min_no
    ) %>% View()
