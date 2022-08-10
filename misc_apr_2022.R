
library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
library(stringr)
library(ggplot2)
source("load_afltables.R")
source("load_fryzigg.R")

afltables %>% 
    select(season, playing_for_short, id, first_name, surname, time_on_ground) %>% 
    group_by(season, playing_for_short, id, first_name, surname) %>% 
    summarise(
        n = n(),
        hundred = sum(time_on_ground == 100)
    ) %>% View()

afltables %>% 
    select(season, round, playing_for_short,opp,pq_4_g,pq_4_b,playing_for_score,opp_score) %>% 
    distinct() %>% 
    View()

gk_acc <- afltables %>% 
    filter(season > 1965) %>% 
    group_by(season, id, first_name, surname) %>% 
    summarise(
        sg = sum(goals),
        sb = sum(behinds),
        acc = sg/(sb+sg)*100,
        attempts = sb+sg
    ) %>% 
    filter(attempts>0)

gk_acc %>% 
    ggplot(aes(x = attempts, y = acc)) +
    geom_point() +
    # scale_x_log10() +
    geom_smooth() +
    facet_wrap('season')

afltables %>% 
    select(season, round, playing_for, playing_for_score) %>% 
    distinct() %>% 
    group_by(season, round) %>% 
    summarise(
        n_teams  = n(),
        t_points = sum(playing_for_score),
        avg_per_team = t_points/n_teams
    ) %>% View()

afltables %>% 
    select(season, round, playing_for, w_l) %>% 
    filter(w_l == 'W', round %in% c(1:5), season > 2010) %>% distinct() %>% 
    group_by(season) %>% 
    summarise(
        n_teams = length(unique(playing_for))
    ) %>% View()

player_str_split <- afltables %>% 
    # filter(season > 2000) %>% 
    select(date, playing_for, id, first_name, surname) %>% 
    unite(full_name, first_name, surname,sep = ' ') %>% 
    mutate(
        full_name_clean = tolower(full_name) %>% str_remove_all('([^a-z])'),
        n_letters = nchar(full_name_clean),
        letters = strsplit(full_name_clean,"")
    ) %>% 
    unnest(letters) %>% 
    distinct()

count_data <- 
    inner_join(
        player_str_split,player_str_split,
        by = c('date','playing_for','n_letters','letters'),suffix = c('','_2')
    ) %>% 
    filter(id > id_2) %>% 
    group_by(date, playing_for, n_letters, full_name, full_name_2) %>% 
    count() %>% ungroup() %>% 
    filter(n_letters - 1 == n, full_name != full_name_2)

count_data %>% 
    group_by(playing_for, full_name, full_name_2) %>% 
    summarise(
        n_times = n(),
        first = min(year(date)),
        last  = max(year(date))
    ) %>% View()

afltables %>% 
    select(id, first_name, surname, kicks, handballs, marks, tackles) %>% 
    filter(
        kicks >= 10,
        handballs >= 10,
        marks >= 10 | tackles >= 10
    ) %>% 
    group_by(id, first_name, surname) %>% count() %>% View()

quarters_ladder <- afltables %>% 
    filter(season == 2022) %>% 
    select(season, date, round, playing_for, opp, starts_with('pq'),starts_with('oq')) %>% 
    distinct() %>% 
    mutate(
        ps_1 = pq_1_g*6 + pq_1_b,
        ps_2 = (pq_2_g-pq_1_g)*6 + (pq_2_b-pq_1_b),
        ps_3 = (pq_3_g-pq_2_g)*6 + (pq_3_b-pq_2_b),
        ps_4 = (pq_4_g-pq_3_g)*6 + (pq_4_b-pq_3_b),
        os_1 = oq_1_g*6 + oq_1_b,
        os_2 = (oq_2_g-oq_1_g)*6 + (oq_2_b-oq_1_b),
        os_3 = (oq_3_g-oq_2_g)*6 + (oq_3_b-oq_2_b),
        os_4 = (oq_4_g-oq_3_g)*6 + (oq_4_b-oq_3_b),
        .keep = 'unused'
    ) %>% 
    pivot_longer(
        cols = ps_1:os_4,
        names_to = c('team','quarter'),
        names_pattern = "(.*)_(.*)",
        values_to = 'score'
    ) %>% 
    pivot_wider(
        names_from  = team,
        values_from = score
    ) %>% 
    mutate(
        w_l = case_when(
            ps > os ~ 'W',
            ps < os ~ 'L',
            T ~ 'D'
        )
    ) %>% 
    group_by(quarter, playing_for, w_l) %>% 
    summarise(
        s_for = sum(ps),
        s_opp = sum(os),
        games = n(),
        .groups = 'drop'
    ) %>% 
    pivot_wider(
        names_from = w_l,
        values_from = games,
        values_fill = list(games = 0)
    ) %>% 
    group_by(quarter, playing_for) %>% 
    summarise(
        W = sum(W),
        D = sum(D),
        L = sum(L),
        `%` = round(sum(s_for)/sum(s_opp)*100, 2),
        .groups = 'drop'
    ) %>%
    mutate(P = W*4 + D*2) %>% arrange(as.numeric(quarter),-P, -`%`) %>% 
    group_by(quarter) %>% mutate(pos = row_number())

write.csv(quarters_ladder, 'custom_ladders/quarters_ladder.csv', row.names = F)

afltables %>% 
    filter(season > 2000) %>% 
    select(date, round, home_team, away_team, id, first_name, surname, fantasy_points) %>% 
    mutate(
        first_init = substr(first_name,1,1) %>% tolower(),
        full_name  = paste(first_init, surname)
    ) %>% 
    group_by(date, round, home_team, away_team, full_name, fantasy_points) %>% 
    count() %>% View()

owen_five <- afltables %>% 
    select(season, date, round, playing_for, w_l) %>% 
    distinct() %>% 
    filter(round %in% 1:5, w_l == 'W') %>% 
    group_by(season, playing_for) %>% 
    count() %>% filter(n == 5)

afltables %>% 
    select(season, date, round, playing_for, opp, w_l, playing_for_score, opp_score) %>% 
    distinct() %>% 
    filter(round == 6, w_l == 'L') %>% 
    mutate(margin = playing_for_score - opp_score) %>% 
    inner_join(owen_five, by = c('season','playing_for')) %>% 
    group_by(season) %>% 
    count() %>% View()

afltables %>% 
    select(season, round, id, first_name, surname, goals, behinds) %>% 
    filter(goals == 1) %>% View()

afltables %>% 
    select(season, date, round, playing_for_short, opp_short, pq_4_g, pq_4_b, playing_for_score, opp_score) %>% 
    distinct() %>% 
    mutate(margin = playing_for_score - opp_score) %>% 
    filter(margin == pq_4_b) %>% View()

afltables %>% 
    filter(season > 1980) %>% 
    select(season, round, date, home_team, away_team, playing_for, id, first_name, surname, disposals) %>% 
    group_by(season, round, date, home_team, away_team) %>% 
    arrange(desc(disposals)) %>% 
    mutate(n = row_number()) %>% 
    filter(n %in% 1:2) %>% 
    mutate(diff = max(disposals)-min(disposals)) %>% View()

afltables %>% 
    filter(season == 1992) %>% 
    select(date, id, first_name, surname, behinds) %>% 
    mutate(b_tf = behinds > 0) %>% 
    group_by(id, first_name, surname) %>% 
    mutate(sum = cumsum(b_tf)) %>% ungroup() %>% 
    filter(sum == date) %>% group_by(date) %>% count()

afltables %>% 
    filter(round %in% 1:25) %>% 
    select(season, id, first_name, surname, behinds) %>% 
    mutate(b_tf = behinds > 0) %>% 
    group_by(season, id, first_name, surname) %>% 
    summarise(
        n   = n(),
        g_b = sum(b_tf)
    ) %>% ungroup() %>% 
    filter(n-1 == g_b) %>% View()

afltables %>% 
    select(season, round, playing_for, pq_4_g, pq_4_b,w_l,playing_for_score,opp_score) %>%
    distinct() %>% 
    mutate(margin = playing_for_score - opp_score) %>% 
    filter(w_l == 'W', margin == pq_4_b) %>% View()

afltables %>% 
    select(season, round, playing_for, pq_4_g, pq_4_b, w_l) %>% 
    distinct() %>% 
    filter(pq_4_b >= 20) %>% 
    group_by(w_l) %>% count()

afltables %>% 
    select(season, round, playing_for, playing_for_score) %>% distinct() %>% 
    group_by(season, playing_for, playing_for_score) %>% 
    count() %>% View()

afltables %>% 
    filter(season >= 1999) %>% 
    select(id, first_name, surname, bounces) %>% 
    mutate(b_tf = bounces > 0) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        n = n(),
        sb= sum(b_tf)
    ) %>% View()
    
afltables %>% 
    select(season, date,round, playing_for, playing_for_score, opp_score) %>% 
    distinct() %>% 
    mutate(margin = playing_for_score - opp_score) %>% 
    group_by(playing_for) %>% 
    mutate(
        prev_game = lag(margin), 
        prev_2_game = lag(margin, 2),
        prev_3_game = lag(margin, 3),
        prev_4_game = lag(margin, 4),
        prev_5_game = lag(margin, 5),
        cumul_res = margin + prev_game + prev_2_game + prev_3_game #+ prev_4_game #+ prev_5_game
        # cumul_res = margin + prev_game + prev_2_game
    ) %>% View()
    
afltables %>% 
    filter(season > 1960) %>% 
    select(id, first_name, surname, date, venue, w_l) %>%
    filter(w_l == 'W') %>% 
    group_by(id, first_name, surname, venue) %>% 
    arrange(date) %>% 
    mutate(date_diff = date - lag(date)) %>% View()

as.Date('2022-05-01') -    as.Date('2009-08-29')

crowd_fp <- afltables %>% 
    filter(season > 2000) %>% 
    select(attendance, id, first_name, surname, fantasy_points) %>% 
    mutate(log_att = log(attendance+1)) %>% 
    group_by(id) %>% 
    mutate(games = n()) %>% ungroup() %>% filter(games > 50)

fit_data <- crowd_fp %>%
    nest_by(id, first_name, surname) %>%
    mutate(mod = list(lm(fantasy_points ~ log_att, data = data))) %>%
    summarise(broom::tidy(mod)) %>% 
    filter(term == 'log_att')

ggplot(crowd_fp %>% filter(id == 11772), aes(x = attendance, y = fantasy_points)) +
    geom_smooth(method = 'lm') +
    geom_point() +
    scale_x_log10()

afltables %>% 
    select(season, round, playing_for, id, first_name, surname) %>% 
    mutate(fir = tolower(first_name) %>%  base::strtrim(2)) %>% 
    group_by(season, round, playing_for, fir) %>% 
    summarise(
        n = n(),
        full_names = paste(first_name, surname, collapse = ', ')
    ) %>% View()

afltables %>% 
    group_by(first_name) %>% 
    filter(season > 1964) %>% 
    summarise(
        avg_disp = mean(disposals),
        avg_goal = mean(goals)
    ) %>% View()

afltables %>% 
    distinct(id, first_name, surname) %>% View()

fryzigg_data %>% 
    distinct(id, first_name, surname, height_cm) %>% 
    filter(
        str_starts(first_name, 'O') | str_starts(surname, 'O')
    ) %>% drop_na() %>% View()

afltables %>% 
    select(date,round,season,id, first_name,surname,playing_for,opp, goals, disposals) %>% 
    filter(wday(date) == 6, day(date) == 13) %>% View()

afltables %>% 
    select(season, id, first_name, surname, frees_against) %>% 
    group_by(season, id, first_name, surname) %>% 
    summarise(
        n = n(),
        tfa = sum(frees_against >= 1)
    ) %>% ungroup() %>% 
    filter(n == tfa) %>% View()

rle_tbl <- function(col) {
    rle_res <- rle(col)
    data_frame(
        len = rle_res$lengths,
        val = rle_res$values
    )
}

consecutive_results <- afltables %>% 
    filter(season > 1964) %>% 
    select(season, date, id, first_name, surname, frees_against) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(
        fa_tf = frees_against >= 1
    ) %>%
    nest(-id, -first_name, -surname) %>% 
    mutate(
        rle_res = purrr::map(data,~rle_tbl(col = .x$fa_tf))
    ) %>% 
    unnest(rle_res)

View(consecutive_results)
