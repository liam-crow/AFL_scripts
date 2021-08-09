library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
source("load_afltables.R")
source("fryzigg_data.R")

exp_data <- afltables %>% 
    select(season, round, date, home_team, away_team, h_a, id, first_name, surname, home_score, away_score) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(games_played = row_number()) %>% 
    group_by(season, round, date, home_team, away_team, h_a) %>% 
    summarise(
        games_played = sum(games_played) - sum(n()),
        home_score = unique(home_score),
        away_score = unique(away_score),
        .groups = 'drop'
    ) %>%
    pivot_wider(
        names_from = h_a,
        values_from = games_played
    ) %>% 
    mutate(H_exp_diff = H - A, H_margin = home_score - away_score, total_exp = H + A)

library(ggplot2)
ggplot(exp_data, aes(H_exp_diff, H_margin)) +
    geom_point() +
    geom_smooth()

fit1 <- lm(H_exp_diff ~ H_margin, exp_data)
summary(fit1)

ggplot(exp_data, aes(x = date, y = total_exp)) +
    geom_point() +
    geom_smooth()

afltables %>% 
    select(date, round, home_team, away_team, h_a, playing_for, id, first_name, surname, goals) %>% 
    group_by(date, round, home_team, away_team, h_a, playing_for) %>% 
    filter(goals == max(goals)) %>% mutate(n = n()) %>% filter(n == 1) %>%
    group_by(date, round, home_team, away_team) %>% 
    mutate(n = n()) %>% filter(n == 2) %>% 
    pivot_wider(
        id_cols = c(date, round, home_team, away_team),
        names_from = h_a,
        values_from = c(first_name, surname)
    ) %>% 
    filter(
        first_name_A == surname_H || first_name_H == surname_A
    ) %>% 
    select(date, round, home_team, away_team, first_name_H, surname_H, first_name_A, surname_A) %>% View()

afltables %>% 
    select(season, date, round, home_team, away_team, playing_for, w_l, goal_assists) %>%
    filter(w_l == 'W', season >= 2003) %>% 
    group_by(date, round, home_team, away_team, playing_for, w_l) %>% 
    summarise(
        tga = sum(goal_assists)
    ) %>% View()

afltables %>% 
    select(season, date, round, home_team, away_team, playing_for, id, first_name, surname, goals, behinds) %>%
    filter(!(goals == 0 & behinds == 0), season >= 1965) %>% 
    group_by(season, date, round, home_team, away_team, goals, behinds) %>% 
    summarise(
        n = n(),
        comb = paste(first_name, surname, collapse = ', ')
    ) %>% View()

afltables %>% 
    select(season, round, date, home_team, away_team, id, first_name, surname) %>% 
    group_by(season, round, date, home_team, away_team, surname) %>% 
    filter(n() %% 2 == 0) %>% 
    summarise(
        n = n(),
        comb = paste0(first_name, collapse = ', ')
    ) %>% 
    group_by(season, round, date, home_team, away_team) %>% 
    mutate(n = n()) %>% View()

afltables %>% 
    select(season, round, date, home_team, away_team, playing_for, goals) %>% 
    filter(goals > 0) %>% 
    group_by(season, round, date, home_team, away_team) %>% 
    count() %>% View()

afltables %>% 
    select(season, round, date, home_team, away_team, first_name, surname, hit_outs) %>% 
    filter(hit_outs > 0) %>% 
    group_by(season, round, date, home_team, away_team, first_name) %>% 
    summarise(
        n = n(),
        s_ho = sum(hit_outs)
    ) %>% 
    View()

afltables %>% 
    select(season, round, home_team, away_team) %>% 
    distinct() %>% 
    filter(round %in% c(1:30), season > 1965) %>% 
    group_by(season, round) %>% count() %>% 
    View()

fryzigg_data %>% 
    select(season, match_round, date, match_home_team, match_away_team, match_winner, player_id, player_first_name, player_last_name, disposals, disposal_efficiency_percentage) %>% 
    filter(disposal_efficiency_percentage %in% c(0,100)) %>% 
    View()

bible_names <- c('caleb','james','levi','tom','thomas','ethan','noah','jacob',
                 'isaac','eli','zach','zac','andrew','andy','sam','samuel',
                 'jonah','simon','isaiah','joshua','josh','luke','nathan','benjamin',
                 'ben','john','aaron','adam','matthew','matt','jared','michael',
                 'mike','daniel','dan')
afltables %>% 
    select(season, round, date, home_team, away_team, id, first_name, surname) %>% 
    mutate(first_name = tolower(first_name)) %>% 
    filter(first_name %in% bible_names) %>% 
    group_by(season, round, date, home_team, away_team) %>% 
    count() %>% 
    View()

fryzigg_data %>% 
    select(season, match_round, date, match_home_team, match_away_team, match_winner, 
           player_id, player_first_name, player_last_name, disposals, score_involvements) %>% View()

afltables %>% 
    select(season, round, date, playing_for, opp) %>% distinct() %>% 
    mutate(day = lubridate::day(date)) %>% 
    group_by(season, playing_for, opp) %>% 
    mutate(same_day_tf = day == lag(day)) %>% 
    # filter(playing_for == 'Richmond', opp == 'Melbourne') %>% View()
    summarise(
        consecutive = rle_tbl(same_day_tf)
    ) %>% 
    View()

afltables %>% 
    select(season, round, date, playing_for, opp) %>% distinct() %>% 
    mutate(day = lubridate::day(date)) %>% 
    group_by(season, day, playing_for, opp) %>% 
    count() %>% 
    View()

afltables %>% 
    select(season, round, date, playing_for, opp) %>% distinct() %>% 
    mutate(month = lubridate::month(date)) %>% 
    group_by(playing_for, opp) %>% 
    mutate(same_month_tf = month == lag(month)) %>% 
    filter(playing_for == 'Collingwood', opp == 'Melbourne') %>% 
    summarise(
        consecutive = rle_tbl(same_month_tf)
    ) %>% 
    View()  

afltables %>% 
    select(season, round, date, id, first_name, surname) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(games_played = row_number()) %>% 
    filter(games_played == 1) %>% 
    group_by(season) %>% 
    count() %>% View()
    
afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname, marks, disposals) %>% 
    filter(marks > disposals) %>% 
    mutate(diff = marks - disposals) %>% View()

afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname) %>% 
    filter(grepl('zz', surname)) %>% 
    group_by(season, round, date, playing_for) %>% 
    summarise(
        n = n(),
        comb = paste0(surname, collapse = ', ')
    ) %>% View()

afltables %>% 
    select(season, round, date, venue, playing_for, id, first_name, surname, w_l, goals) %>% 
    filter(grepl('zz', surname)) %>% 
    group_by(id, first_name, surname, venue, w_l) %>% 
    summarise(
        tg = sum(goals),
        n = n()
    ) %>% 
    pivot_wider(
        names_from = c(w_l),
        values_from = n,
        values_fill = 0
    ) %>% 
    mutate(wr = round((W+D)/(W+D+L)*100,2)) %>% 
    View()

afltables %>% 
    select(season, round, date, home_team, away_team, playing_for, id, first_name, surname, jumper_no, hit_outs) %>% 
    filter(hit_outs > 15) %>% 
    group_by(season, round, date, home_team, away_team, jumper_no) %>% 
    count() %>% View()

afltables %>% 
    select(season, round, date, home_team, away_team, hq_1_g, hq_2_g, hq_3_g, hq_4_g, aq_1_g, aq_2_g, aq_3_g, aq_4_g) %>% 
    distinct() %>% 
    filter(
        hq_1_g == aq_1_g,
        hq_2_g == aq_2_g,
        hq_3_g == aq_3_g,
        hq_4_g == aq_4_g
    ) %>% 
    View()

afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname, kicks, handballs) %>% 
    filter(handballs > 0, kicks > 0) %>% 
    mutate(k2h = kicks/handballs) %>% View()
