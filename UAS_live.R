
library(dplyr)
library(tidyr)
source("load_afltables.R")
source("fryzigg_data.R")

fryzigg_data %>% 
    select(season, match_round, player_id, player_first_name, player_last_name, player_height_cm, player_weight_kg,
           player_team, disposals) %>% 
    filter(player_height_cm <= 175) %>% View()

afltables %>% 
    select(date, season, round, playing_for, 
           pq_1_g, pq_1_b, pq_2_g, pq_2_b, pq_3_g, pq_3_b, pq_4_g, pq_4_b,
           oq_1_g, oq_1_b, oq_2_g, oq_2_b, oq_3_g, oq_3_b, oq_4_g, oq_4_b) %>% distinct() %>% 
    mutate(quarters_won = pq_1_g *6 + pq_1_b)

afltables %>% 
    select(season, round, playing_for, opp, id, first_name, surname, goals, behinds, opp_score) %>% 
    # filter(opp == "Geelong") %>% 
    mutate(p_score = goals * 6 + behinds) %>% 
    filter(p_score > opp_score) %>% View()

afltables %>%
    filter(season > 1964) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(s_d = sum(disposals), s_g = n()) %>% 
    filter(s_g > 150) %>% View()

afltables %>% 
    filter(season > 1930) %>% 
    select(date, id, first_name, surname) %>% 
    group_by(id, first_name, surname) %>% arrange(date) %>% 
    mutate(games_played = row_number()) %>% 
    filter(games_played %in% c(300,400)) %>% 
    mutate(diff = date -lag(date)) %>% View()

afltables %>% 
    filter(season > 1930) %>% 
    select(date, playing_for, id, first_name, surname, goals) %>% 
    group_by(id, first_name, surname) %>% arrange(date) %>% 
    mutate(games_played = row_number()) %>% 
    filter(games_played == 100) %>% View()

afltables %>% 
    select(date, round, playing_for, w_l) %>% distinct() %>% 
    filter(round == 'GF' & w_l == 'W') %>% 
    arrange(date) %>% 
    mutate(diff = date - lag(date)) %>% View()

afltables %>% distinct() %>%
    select(season,date, id, first_name, surname, opp, brownlow_votes, w_l) %>%
    filter(w_l == 'L') %>%
    group_by(season, id, first_name, surname, opp) %>% 
    summarise(sum_bv = sum(brownlow_votes)) %>% ungroup() %>% 
    arrange(-season,-sum_bv) %>% View()
    # filter(sum_bv == 5) %>% 
    # mutate(res = paste0(season,' ',first_name,' ', surname, ' vs ', opp,': ', sum_bv)) %>% select(res) %>% View()

