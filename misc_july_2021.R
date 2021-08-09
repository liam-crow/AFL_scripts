library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
source("load_afltables.R")
source("fryzigg_data.R")


# rich gc
afltables %>% 
    select(season, round, date, home_team, away_team, hq_1_g, hq_2_g, hq_3_g, hq_4_g, aq_1_g, aq_2_g, aq_3_g, aq_4_g, home_score, away_score) %>% 
    mutate(margin = home_score - away_score) %>% 
    distinct() %>% 
    filter(
        hq_1_g == aq_1_g,
        hq_2_g == aq_2_g,
        hq_3_g == aq_3_g,
        hq_4_g == aq_4_g
    ) %>% 
    View()

afltables %>% 
    select(season, date, round, playing_for, id, first_name, surname, possessions, goals, behinds) %>% 
    filter(goals == 0 & behinds == 0) %>% 
    arrange(-possessions) %>% 
    View()

afltables %>% 
    select(season, date, round, playing_for, id, first_name, surname, possessions, goals, behinds) %>% 
    group_by(season,id, first_name, surname) %>% 
    summarise(
        t_pos = sum(possessions),
        t_g = sum(goals),
        t_b = sum(behinds)
    ) %>% 
    filter(t_g == 0 & t_b == 0) %>% 
    arrange(-t_pos) %>% 
    View()

afltables %>% 
    select(season, date, round, playing_for, opp, pq_4_g, pq_4_b, playing_for_score, oq_4_g, oq_4_b, opp_score, w_l) %>% 
    distinct() %>% 
    mutate(
        p_scoring_shots = pq_4_g + pq_4_b, 
        o_scoring_shots = oq_4_g + oq_4_b,
        scoring_diff = p_scoring_shots - o_scoring_shots
    ) %>% 
    filter(w_l == 'L') %>% 
    arrange(-scoring_diff) %>% 
    View()

afltables %>% 
    select(season, date, round, playing_for, opp, pq_4_g, pq_4_b, playing_for_score, oq_4_g, oq_4_b, opp_score, w_l) %>% 
    distinct() %>% 
    mutate(
        p_scoring_shots = pq_4_g + pq_4_b, 
        o_scoring_shots = oq_4_g + oq_4_b,
        scoring_diff = p_scoring_shots - o_scoring_shots
    ) %>% 
    filter(w_l == 'L') %>% 
    group_by(season, round) %>% count() %>% View()


afltables %>% 
    select(season, round, home_team, away_team, id, first_name, surname) %>% 
    group_by(season, round, first_name) %>% 
    mutate(nf = n()) %>% 
    group_by(season, round, surname) %>% 
    mutate(ns = n()) %>% 
    group_by(season, round, home_team, away_team) %>% 
    filter(all(ns) == 1) %>% View()

player_sc_data <- fryzigg_data %>% 
    select(season, match_round, player_first_name, player_last_name, supercoach_score) %>% 
    drop_na()

player_sc_data_game <- fryzigg_data %>% 
    select(season, match_round, match_home_team, match_away_team, player_first_name, player_last_name, supercoach_score) %>% 
    drop_na()

inner_join(
    player_sc_data, player_sc_data, 
    by = c("season", "match_round", "player_last_name" = "player_first_name")
) %>% 
    mutate(total = supercoach_score.x + supercoach_score.y) %>% View()

inner_join(
    player_sc_data_game, player_sc_data_game, 
    by = c("season", "match_round", "match_home_team", "match_away_team", "player_last_name" = "player_first_name")
) %>% 
    mutate(total = supercoach_score.x + supercoach_score.y) %>% View()


afltables %>% 
    select(season, date, playing_for, opp, id, first_name, surname, goals, behinds, playing_for_score) %>% 
    mutate(ind_score = goals*6 + behinds) %>% 
    group_by(playing_for, opp, id, first_name, surname) %>% 
    summarise(
        team_score = sum(playing_for_score),
        ind_t = sum(ind_score)
    ) %>% 
    filter((playing_for == 'Richmond' & opp == "St Kilda") || (playing_for == 'St Kilda' & opp == "Richmond"), surname == 'Riewoldt') %>% 
    mutate(pcnt = round(ind_t/team_score*100,2)) %>% ungroup() %>% 
    select(-id) %>% 
    View()

afltables %>% 
    group_by(opp, id, first_name, surname) %>% 
    count() %>% View()

init_data <- afltables %>% 
    select(id, first_name, surname) %>% distinct() %>% 
    mutate(
        first_init = strtrim(first_name,1),
        second_init= strtrim(surname,1),
        init_comb = paste0(first_init, second_init)
    ) %>% 
    group_by(init_comb) %>% 
    count() %>% ungroup() %>% 
    mutate(init_comb = forcats::fct_reorder(init_comb, n, .desc = F)) %>% 
    top_n(n = 20)

library(ggplot2)

ggplot(init_data) +
    geom_bar(aes(n, init_comb), stat = "identity")

afltables %>% 
    select(season, round, date, id, first_name, surname, inside_50_s, goals, behinds) %>% 
    drop_na() %>% 
    filter(season >= 1998, inside_50_s == 0) %>% 
    View()

afltables %>% 
    select(season, round, date, playing_for, jumper_no) %>% 
    filter(season >= 1965) %>% 
    group_by(season, round, date, playing_for) %>% 
    summarise(
        n = n(),
        min_no = min(jumper_no),
        max_no = max(jumper_no)
    ) %>% 
    filter(min_no == 1) %>% View()
    
afltables %>% 
    select(season, round, id, first_name, surname, games_played) %>% 
    filter(games_played %in% c(1:10*50)) %>% 
    group_by(id, first_name, surname, round) %>% 
    summarise(
        n = n(),
        comb = paste(games_played, collapse = ", ")
    ) %>% View()

afltables %>% 
    select(season, round, games_played, id, first_name, surname) %>% 
    filter(games_played %in% c(1:10*50)) %>% distinct() %>% 
    group_by(season, round) %>% 
    summarise(
        n = n(),
        comb = paste(first_name, surname, games_played, collapse = ", ")
    ) %>% View()


# Via Essendon Fans Blaming the Umpires Instagram - 
# Ken Dredge - love to know our win/loss with Razor Ray umpiring.

afltables %>% 
    select(season, playing_for, w_l, umpire_1, umpire_2, umpire_3, umpire_4) %>% 
    distinct() %>% 
    filter(season >= 2004) %>% 
    mutate(
        ray_tf = if_else(
        umpire_1 == 'Ray Chamberlain' | 
        umpire_2 == 'Ray Chamberlain' |
        umpire_3 == 'Ray Chamberlain' |
        umpire_4 == 'Ray Chamberlain', T, F)
        ) %>% View()
    group_by(playing_for, w_l, ray_tf) %>% 
    count() %>% 
    
    View()

afltables %>% 
    select(id, first_name, surname) %>% 
    distinct() %>% View()
    