library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
source("load_afltables.R")
source("fryzigg_data.R")

afltables %>% 
    select(season, id, first_name, surname, brownlow_votes, jumper_no) %>% 
    group_by(season, id, first_name, surname, jumper_no) %>% 
    summarise(
        sum_bv = sum(brownlow_votes)
    ) %>% 
    group_by(season) %>% 
    filter(sum_bv == max(sum_bv)) %>% 
    filter(sum_bv != 0) %>% 
    group_by(jumper_no) %>% 
    summarise(
        n = n(),
        names = paste(first_name, surname, collapse = ', ')
    ) %>% View()

afltables %>% 
    select(season, round, date, playing_for, opp, pq_3_g, pq_3_b, playing_for_score) %>% distinct() %>% 
    mutate(q3_score = pq_3_g*6 + pq_3_b) %>% 
    filter(q3_score *2 == playing_for_score) %>% View()

afltables %>% 
    select(season, round, date, playing_for, opp, playing_for_score, opp_score) %>% distinct() %>% 
    mutate(margin = playing_for_score - opp_score) %>% 
    filter(playing_for == 'Carlton', margin >= 100) %>% 
    arrange(desc(date)) %>% 
    View()

afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname, playing_for_score, opp_score) %>% distinct() %>% 
    mutate(margin = playing_for_score - opp_score) %>%
    filter(margin <= -100) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(n = n()) %>% View()

afltables %>% 
    select(season, round, date, id, first_name, surname, disposals, w_l) %>% 
    group_by(id, first_name, surname, w_l) %>% 
    summarise(
        n = n(),
        t_disp = sum(disposals)
    ) %>% View()

afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname, disposals) %>% 
    group_by(season, round, date, playing_for, first_name) %>% 
    summarise(
        n = n(),
        tdisp = sum(disposals)
    ) %>% View()

names(fryzigg_data)
fryzigg_data %>% 
    select(season, match_round, date, match_home_team, match_away_team, match_weather_type) %>% distinct() %>% 
    group_by(match_weather_type) %>% 
    summarise(
        n = n(),
        mdate = min(date)
    )

afltables %>% 
    select(season, round, date, local_start_time, home_team, away_team) %>% distinct() %>% 
    View()

afltables %>% 
    select(season, round, home_team, away_team, playing_for, starts_with('pq')) %>% distinct() %>% 
    View()

afltables %>% 
    select(season, round, home_team, away_team, playing_for, pq_1_g, pq_2_g, pq_3_g, pq_4_g, w_l) %>% distinct() %>% 
    mutate(
        q1g = pq_1_g,
        q2g = pq_2_g - pq_1_g,
        q3g = pq_3_g - pq_2_g,
        q4g = pq_4_g - pq_3_g,
    ) %>% 
    filter(q1g >= 10 | q2g >= 10 | q3g >= 10 | q4g >= 10, w_l == 'L') %>% 
    View()

afltables %>% 
    select(season, round, date, playing_for, opp, id, first_name, surname, goals, w_l) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(games_played = row_number()) %>% 
    filter(games_played == 1, w_l == 'L', goals > 4) %>% View()

afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname) %>% 
    group_by(season, round, date, playing_for) %>% 
    summarise(
        n = n(),
        names_full = paste(surname, collapse = ', '),
        names_comb = tolower(paste0(surname, collapse = '')),
        n_char = length(unlist(strsplit(names_comb, split = '')))
    ) %>% select(-names_comb) %>% arrange(-n_char) %>% View()

afltables %>% 
    select(season, round, date, playing_for, first_name) %>%
    group_by(season, round, date, playing_for) %>% 
    summarise(names = tolower(paste0(substring(first_name, 1, 1), collapse = ''))) %>% 
    mutate(sw_a = (grepl('a',names))) %>% 
    group_by(playing_for, sw_a) %>% 
    count() %>% 
    pivot_wider(
        names_from = sw_a,
        values_from = n
    ) %>% 
    mutate(ratio = round(`FALSE`/(`TRUE` +`FALSE`)*100, 2)) %>% View()

afltables %>% 
    select(season, round, date, playing_for, opp, playing_for_score, opp_score) %>% distinct() %>% 
    mutate(margin = playing_for_score - opp_score) %>% 
    filter(season >= 2000) %>% 
    group_by(playing_for, opp) %>% 
    summarise(t_margin = sum(margin)) %>% View()
    
afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname, w_l) %>% 
    filter(playing_for == "Collingwood", season >= 2012) %>% 
    group_by(id, first_name, surname, w_l) %>% count() %>% 
    pivot_wider(
        names_from = w_l,
        values_from = n,
        values_fill = 0
    ) %>% 
    mutate(
        win_rate = round(W/(W+D+L)*100,2)
    ) %>% View()

afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname, disposals, marks) %>% 
    View()

afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname, goals) %>% 
    filter(goals > 0) %>% 
    mutate(initial = tolower(substring(first_name, 1, 1))) %>% 
    group_by(season, round, date, playing_for, initial) %>% 
    count() %>% View()

afltables %>% 
    select(date, season ,round, playing_for, playing_for_score, w_l) %>% distinct() %>% View()

library(ggplot2)

frees_data <- afltables %>% 
    select(season, round, date, h_a, home_team, away_score, playing_for, disposals, frees_for, frees_against) %>% 
    filter(season > 2000) %>%
    group_by(season, round, date, h_a, home_team, away_score, playing_for) %>% 
    summarise(
        ff = sum(frees_for), 
        fa = sum(frees_against),
        disp = sum(disposals),
        .groups = 'drop'
    ) %>% 
    pivot_wider(
        names_from = h_a,
        values_from = c(playing_for, disp, ff, fa)
    ) %>% 
    mutate(
        disp_pm = disp_H - disp_A
    ) %>% 
    select(season, round, date, home_team, ff_H, fa_H, disp_pm) %>% 
    pivot_longer(
        cols = c('ff_H', 'fa_H'),
        names_to = 'frees_type',
        values_to = 'n'
    )

ggplot(frees_data, aes(x = disp_pm, y = n, colour = frees_type)) +
    geom_point() +
    geom_smooth()

afltables %>% 
    select(season, date, round, playing_for, id, first_name, surname, goal_assists) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(games_played = row_number()) %>% 
    filter(games_played %in% 1:9, goal_assists > 0) %>% 
    summarise(n = n()) %>% arrange(-n)

afltables %>% 
    select(season, date, round, playing_for, opp, id, first_name, surname) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(games_played = row_number()) %>% 
    filter(games_played %in% c(50*1:3)) %>% 
    group_by(id, first_name, surname, opp) %>% count() %>% View()

afltables %>% 
    select(season, round, date, playing_for, pq_2_g, pq_2_b, oq_2_g, oq_2_b, w_l, playing_for_score, opp_score) %>% distinct() %>% 
    mutate(pscore_ht = pq_2_g *6 + pq_2_b, oscore_ht = oq_2_g *6 + oq_2_b) %>% 
    mutate(
        ht_margin = pscore_ht - oscore_ht,
        margin = playing_for_score - opp_score
    ) %>% 
    View()

afltables %>% 
    select(season, round, date, playing_for, pq_1_g, pq_2_g, pq_3_g, pq_4_g, w_l, playing_for_score, opp_score) %>% 
    distinct() %>% 
    mutate(
        margin = playing_for_score - opp_score
    ) %>% 
    View()

afltables %>% 
    select(season, round, date, playing_for, opp, pq_1_g, pq_1_b, oq_1_g, oq_1_b, w_l, playing_for_score, opp_score) %>% 
    distinct() %>% View()

afltables %>% 
    select(season, date, playing_for, id, first_name, surname, goals) %>% 
    group_by(playing_for, season, id, first_name, surname) %>% 
    summarise(t_goals = sum(goals)) %>% 
    group_by(season) %>% 
    arrange(-t_goals) %>% 
    mutate(pos = row_number()) %>% 
    filter(pos %in% 1:10, playing_for == "Essendon") %>% View()
    group_by(season) %>% count() %>% View()

afltables %>% 
    select(season, round, date, id, first_name, surname, goals, behinds) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(tg = sum(goals), tb = sum(behinds)) %>% View()

unique_name <- afltables %>% 
    select(season, round, date, id, first_name, surname) %>% 
    group_by(first_name) %>% 
    filter(date == min(date), season != 1897)

library(ggplot2)
ggplot(unique_name) +
    geom_violin(aes(x = date, y = 'name')) +
    geom_bar(aes(x = date))

afltables %>% 
    select(season, round, date, home_team, away_team, id, first_name, surname) %>% 
    group_by(season, round, date, home_team, away_team, surname) %>% 
    count() %>% View()

afltables %>% 
    select(season, id, first_name, surname, playing_for, jumper_no) %>% 
    filter(season > 1912, playing_for == 'St Kilda') %>% 
    filter(jumper_no == 29) %>% 
    group_by(id, first_name, surname, playing_for, jumper_no) %>% 
    summarise(games_played = n(), .groups = 'drop') %>% 
    arrange(-games_played) %>% 
    summarise(comb = paste0(first_name,' ',surname,' ',games_played, collapse = ', ')) %>% View()

afltables %>% 
    select(season, round, date, id, home_team, away_team, first_name, surname, jumper_no, hit_outs) %>% 
    filter(jumper_no != 0, hit_outs >= 5) %>% 
    group_by(season, round, date, home_team, away_team, jumper_no) %>% 
    summarise(
        t_ho = sum(hit_outs),
        n = n(),
        comb = paste(first_name, surname, hit_outs,'ho', collapse = ', ')
    ) %>% View()

afltables %>% 
    select(season, date, round, playing_for, id, first_name, surname, tackles) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(games_played = row_number()) %>% 
    ungroup() %>% 
    filter(games_played == 1) %>% 
    View()
    
team_mate_goals <- afltables %>% 
    select(season, round, playing_for, id, bounces) %>% 
    mutate(id = as.character(id)) %>% 
    mutate(id_2 = id) %>% 
    filter(bounces >= 1) %>% select(-bounces) %>% 
    group_by(season, round, playing_for) %>% 
    complete(id, id_2) %>% ungroup() %>% 
    filter(id != id_2, id > id_2) %>% 
    group_by(id, id_2) %>% 
    count()

player_ids <- afltables %>% 
    select(id, first_name, surname) %>% distinct() %>% 
    mutate(id = as.character(id))

team_mate_goals %>% inner_join(player_ids, by = c('id' = 'id')) %>% 
    inner_join(player_ids, by = c('id_2' = 'id')) %>% View()

afltables %>% 
    select(season, round, date, playing_for_short, id, first_name, surname, bounces) %>% 
    filter(bounces >= 10) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        n = length(unique(playing_for_short)),
        comb = paste(unique(playing_for_short), collapse = ', ')
    ) %>% View()

afltables %>% 
    group_by(season, round, date, home_team, away_team, surname) %>% 
    count() %>% View()

afltables %>% 
    filter(
        season == 2021,
        round %in% c(6,7,8,9,10,11)
    )
afltables %>% 
    select(season, round, date, playing_for, w_l, playing_for_score, opp_score) %>% 
    distinct() %>% 
    filter(playing_for_score - opp_score == 1) %>% 
    group_by(
        season, round
    ) %>% count() %>% View()

afltables %>% 
    select(season, round, date, playing_for, first_name, surname) %>% 
    filter(grepl('ll', first_name, ignore.case = T)) %>% 
    group_by(season, round, date, playing_for) %>% 
    summarise(
        n = n(),
        comb = paste0(surname, collapse = ', ')
    ) %>% View()
    
fryzigg_data %>% 
    select(season, date, match_round, player_team, player_id, player_first_name, player_last_name, spoils) %>% 
    filter(spoils >= 5) %>% View()

fryzigg_data %>% 
    select(season, date, match_round, player_team, player_id, player_first_name, player_last_name, 
           marks, marks_inside_fifty, tackles, tackles_inside_fifty, ground_ball_gets, f50_ground_ball_gets,
           goals, behinds) %>% 
    filter(
        marks == marks_inside_fifty, 
        tackles == tackles_inside_fifty,
        ground_ball_gets == f50_ground_ball_gets,
        marks > 0, tackles > 0, ground_ball_gets >0
    ) %>% View()

fryzigg_data %>% 
    select(player_team, player_id, player_first_name, player_last_name, player_height_cm) %>% distinct() %>% 
    View()

unique(fryzigg_data$player_position)

fryzigg_data %>% 
    select(season, date, match_round, player_team, player_id, player_first_name, player_last_name,
           guernsey_number, player_position) %>% distinct() %>% 
    filter(
        player_position %in% c('C', 'R', 'RR', 'RK')
    ) %>% 
    group_by(
        season, date, match_round, player_team
    ) %>% 
    summarise(
        n = n(),
        total = sum(guernsey_number),
        comb = paste0(player_first_name,' ',player_last_name,' #',guernsey_number, collapse = ', ')
    ) %>% 
    View()

afltables %>% 
    select(playing_for, opp, venue) %>% distinct() %>% 
    group_by(playing_for, opp) %>% 
    summarise(
        n = n(),
        venues = paste0(venue, collapse = ', ')
    ) %>% View()
    
afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname, games_played, fantasy_points) %>% 
    filter(season > 1980, games_played == 1) %>% 
    View()

fryzigg_data %>% 
    select(season, date, match_round, player_team, player_id, player_first_name, 
           player_last_name, supercoach_score, rating_points) %>% 
    group_by(player_id, player_first_name, player_last_name) %>% 
    arrange(date) %>% 
    mutate(games_played = row_number()) %>% 
    filter(games_played == 1, season > 2010) %>% 
    View()

afltables %>% 
    select(season, date, round, playing_for, opp, playing_for_score, frees_for, frees_against) %>% 
    group_by(season, date, round, playing_for, opp, playing_for_score) %>% 
    summarise(
        tff = sum(frees_for),
        tfa = sum(frees_against)
    ) %>% 
    filter(playing_for_score < tff) %>% 
    View()
    
    
afltables %>% 
    select(id, first_name, surname, jumper_no, goals, behinds) %>% 
    mutate(score = goals*6 + behinds) %>% 
    filter(score == jumper_no) %>% View()

afltables %>% 
    select(season, date, round, home_team, away_team, 
           playing_for, id, first_name, surname, venue) %>% 
    arrange(date) %>% 
    group_by(venue, id) %>% 
    mutate(venue_exp = row_number()) %>% 
    group_by(season, date, round, venue, home_team, away_team, playing_for) %>% 
    summarise(
        t_exp = sum(venue_exp),
        .groups = 'drop'
    ) %>% 
    group_by(season, date, round, venue, home_team, away_team) %>% 
    mutate(diff = max(t_exp)-min(t_exp)) %>% 
    View()
