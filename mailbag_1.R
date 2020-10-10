
library(dplyr)
library(tidyr)
source("load_afltables.R")
source("fryzigg_data.R")

# What’s the most amount of touches a player has had in their final game?

afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname, disposals) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(games_played = row_number()) %>% 
    filter(games_played == max(games_played)) %>% View()

afltables %>% 
    select(season, round, date, id, first_name, surname, hit_outs, bounces) %>% 
    filter(hit_outs >= 10) %>% View()

afltables %>% 
    select(id, first_name, surname) %>% distinct() %>% 
    mutate(surname = tolower(surname)) %>% 
    filter(
        grepl('^lake', surname) | grepl('^river', surname) | grepl('^sea', surname) | grepl('^ocean', surname)
    ) %>% 
    summarise(names = paste(first_name, surname, collapse = ', ')) %>% View()

afltables %>% 
    select(season, round, date, home_team, away_team, playing_for, disposals, goals, behinds) %>% 
    group_by(season, round, date, home_team, away_team, playing_for) %>% 
    summarise(
        sdisp = sum(disposals),
        sshots= sum(goals) + sum(behinds),
        .groups = 'drop'
    ) %>% 
    filter(sdisp > 0) %>% 
    group_by(season, round, date, home_team, away_team) %>% 
    mutate(
        ndisp = length(unique(sdisp)),
        nshots= length(unique(sshots))
    ) %>% 
    filter(ndisp == 1 & nshots == 1)
    
afltables %>% 
    select(season, round, date, playing_for, opp, playing_for_score, opp_score) %>% distinct() %>% 
    filter(100<playing_for_score-opp_score | -100>playing_for_score-opp_score) %>% 
    group_by(season) %>% 
    summarise(n = n()) %>% View()

afltables %>% 
    select(season, round, date, home_team, away_team, home_score, away_score) %>% distinct() %>% 
    filter(home_score == 69 & away_score == 69)

afltables %>% 
    select(season, round, date, id, first_name, surname, goals, behinds) %>% View()

afltables %>% 
    select(season, round, date, id, first_name, surname, goals, w_l) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        sg = sum(goals),
        gf_wins = sum(round == 'GF' & w_l == 'W'),
        mdate = max(date),
        n = n()
    ) %>% 
    filter(gf_wins >= sg, gf_wins > 0) %>% View()

afltables %>% 
    select(season, date, id, first_name, surname, disposals) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(
        games_played = row_number(),
        debut = min(date),
        mgp = max(games_played)
    ) %>% 
    filter(season > 1965, games_played %in% c(1,2,3), disposals == 0) %>% 
    group_by(debut, mgp, id, first_name, surname) %>% count() %>% View()
    
afltables %>% 
    select(opp, id, first_name, surname, goals, disposals) %>%
    group_by(opp, id, first_name, surname) %>% 
    summarise(sg = sum(disposals)) %>% View()

fryzigg_data %>% 
    filter(season == 2020) %>% 
    select(season, date, player_id, player_first_name, player_last_name, guernsey_number, player_team,
           contest_def_one_on_ones, contest_def_losses, contest_off_one_on_ones, contest_off_wins) %>% 
    mutate(
        contest_def_wins = contest_def_one_on_ones - contest_def_losses,
        contest_off_losses = contest_off_one_on_ones - contest_off_wins
    ) %>% 
    filter(guernsey_number %in% c(1)) %>%
    group_by(player_team, player_id, player_first_name, player_last_name, guernsey_number) %>% 
    summarise(
        s_wins = sum(contest_off_wins) + sum(contest_def_wins),
        s_loss = sum(contest_off_losses) + sum(contest_def_losses)
    ) %>% 
    arrange((-s_wins)) %>% 
    View()

afltables %>%
    filter(playing_for == 'West Coast', season >= 2008) %>% 
    select(season, round, playing_for, id, first_name, surname, w_l) %>% 
    group_by(season, round, playing_for, w_l) %>% 
    summarise(player_yn = any(first_name == 'Josh' & surname == 'Kennedy'), .groups = 'drop') %>% 
    group_by(player_yn, w_l) %>% count() %>% ungroup() %>% 
    pivot_wider(
        names_from = w_l,
        values_from = n,
        values_fill = 0
    ) %>% 
    mutate(ratio = round(W/(W+L+D)*100,2))
