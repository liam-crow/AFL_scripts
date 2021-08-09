
library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
source("load_afltables.R")
source("fryzigg_data.R")

afltables %>% 
    group_by(id, first_name, surname) %>% 
    summarise(numbers = paste(sort(unique(jumper_no)), collapse = ' ')) %>% View()
    # summarise(n = n())


afldisp <- afltables %>% 
    select(date, id, first_name, surname, disposals) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(debut = min(date), game = row_number(), n = n()) %>% 
    filter(debut >= "1965-01-01") %>% ungroup()

#Least disposals after 5 20 50 100 200 and 300 games
cutoff <- 400
afldisp %>% 
    filter(game %in% c(1:cutoff), n >= cutoff) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(total_disp = sum(disposals)) %>% 
    arrange(total_disp)
    
afltables %>%
    filter(!(playing_for %in% c('University', 'Brisbane Bears','Fitzroy'))) %>% 
    select(playing_for, surname) %>% distinct() %>%
    group_by(surname) %>% count() %>% 
    summarise(n = n(), paste(playing_for = )) %>% View()

afltables %>% 
    select(date, round, id, first_name, surname, jumper_no, playing_for_short, opp, playing_for_score, opp_score, w_l) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(
        game_no = row_number(),
        margin = playing_for_score - opp_score
    ) %>% 
    filter(game_no == abs(margin), game_no == jumper_no) %>% View()
    # filter(game_no %in% c(1,seq(0,500,50)), margin <= -100) %>% 
    # group_by(id, first_name, surname) %>% mutate(n=n()) %>% View()

afl_alpha <- afltables %>% 
    group_by(id, first_name, surname) %>% summarise(debut = min(date),n = n()) %>% 
    rowwise() %>% 
    mutate(
        fullname_raw = paste0(first_name, surname) %>% tolower() %>% trimws(which = 'both'),
        fullname = gsub('[^\\w]','',fullname_raw, perl = T),
        alphabetical = paste0(sort(unlist(strsplit(fullname,''))), collapse = '')
    ) %>% 
    select(-fullname_raw, -fullname)

afl_alpha %>% 
    filter(
        alphabetical == paste0(sort(unlist(strsplit('raid',''))), collapse = '')
    )

afltables %>%
    select(season, round, date, local_start_time, home_team, home_score, away_team, away_score) %>% 
    distinct() %>% 
    group_by(season) %>% 
    filter(date == min(date), local_start_time == min(local_start_time)) %>% 
    mutate(diff = home_score - away_score) %>% View()

afltables %>% 
    select(playing_for)
    group_by(playing_for) %>% 
    arrange(date) %>% 
    mutate(n = n())

afltables %>% 
    select(date, round, id, first_name, surname, disposals, jumper_no) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(games_played = row_number()) %>% ungroup() %>% 
    mutate(res = disposals * jumper_no * games_played) %>% View()

afltables %>% 
    select(season, round, date, home_team, away_team, playing_for, first_name, surname, w_l, opp, playing_for_score, opp_score) %>% 
    filter(
        grepl('line|bag|coke|bump|snow', surname, ignore.case = T)
    ) %>% 
    group_by(season, round, date, home_team, away_team) %>% 
    mutate(n = n()) %>% View()

afltables %>% 
    select(season, date, round, home_team, away_team, home_score, away_score, goals, behinds) %>% 
    mutate(total_score = home_score + away_score) %>% 
    group_by(season, date, round, home_team, away_team, total_score) %>% 
    summarise(max_g = max(goals), max_b = max(behinds)) %>% View()

afltables %>% 
    select(season, round, id, first_name, surname, playing_for, playing_for_score, playing_for_score, opp_score, w_l) %>% 
    filter(round == '1', season > 2000) %>% 
    mutate(margin = playing_for_score - opp_score) %>% 
    group_by(id, first_name, surname, w_l) %>% 
    summarise(
        n = n(),
        total_margin = sum(margin),
        .groups = 'drop'
    ) %>% 
    pivot_wider(
        names_from = w_l,
        values_from = n,
        values_fill = 0
    ) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        total_margin = sum(total_margin),
        L = sum(L),
        W = sum(W),
        D = sum(D),
        .groups = 'drop'
    ) %>% 
    select(id, first_name, surname, total_margin, W, D, L) %>% 
    View()

sample_data %>% 
    mutate(diff = kicks_diff - handballs_diff) %>% View()
