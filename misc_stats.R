
library(dplyr)
source("load_afltables.R")

afltables %>% select(date, id, first_name, surname, goals) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        debut = min(date),
        games = n(),
        goals = sum(goals)
    ) %>% View()

afltables %>% 
    filter(round %in% 1:7, season >= 1965) %>% 
    group_by(season, id, first_name, surname) %>% 
    summarise(
        sum_goals = sum(goals)
    ) %>% 
    group_by(season) %>% 
    filter(sum_goals == max(sum_goals)) %>% 
    arrange(sum_goals)

afltables %>% distinct() %>% 
    select(season, round, playing_for, id, first_name, surname, w_l) %>% 
    filter(
        !(round %in% 1:25),
        first_name == 'Ned'
    )

afltables %>% 
    select(season, round, home_team, away_team, playing_for, first_name, surname, jumper_no) %>% 
    filter(jumper_no == 56) %>% 
    group_by(season, round, home_team, away_team) %>% 
    mutate(n = n()) %>% 
    filter(n == 2) %>% 
    arrange(-n,-season,round)

afltables %>% 
    select(season, round, playing_for, id, first_name, surname, goals, playing_for_score, opp_score) %>% 
    mutate(
        margin = playing_for_score - opp_score
    ) %>% 
    filter(margin < -100) %>% 
    arrange(-goals)

afltables %>% 
    select(season, round, playing_for, pq_4_g, pq_4_b, playing_for_score, w_l) %>% distinct() %>% 
    mutate(acc = pq_4_g/(pq_4_g + pq_4_b)*100) %>% 
    filter(acc > 90) %>% 
    arrange(-playing_for_score)

source("fryzigg_data.R")

fryzigg_data %>% 
    select(
        match_date, match_round, player_id, player_first_name, player_last_name, 
        disposals, disposal_efficiency_percentage
    ) %>% 
    filter(disposals >= disposal_efficiency_percentage) %>% 
    arrange(-disposals) %>% View()

fryzigg_data %>% 
    select(
        season, match_round, player_id, player_first_name, player_last_name,
        supercoach_score, afl_fantasy_score
    ) %>% 
    mutate(diff = supercoach_score - afl_fantasy_score) %>% 
    arrange(-diff)

fryzigg_data %>% 
    select(player_id, player_first_name, player_last_name, player_height_cm, player_weight_kg) %>% distinct() %>% 
    mutate(diff = player_height_cm - player_weight_kg) %>% arrange(diff)
filter(player_weight_kg == 87, player_height_cm == 175)

afltables %>% 
    select(season, round, id, first_name, surname, goals, disposals) %>% 
    filter(
        disposals == round
    ) %>% 
    group_by(season, id, first_name, surname) %>% 
    mutate(count = n()) %>% ungroup() %>% 
    filter(count == max(count)) %>% 
    arrange(-count, id) %>% View()
    
afltables %>% 
    select(season, round, playing_for, starts_with('pq')) %>% distinct() %>% 
    View()

afltables %>% 
    distinct(season, round, date, home_team, away_team, id, first_name, surname, jumper_no) %>% 
    group_by(id, first_name, surname) %>% arrange(date) %>% 
    mutate(games_played = row_number()) %>% ungroup() %>%
    filter(games_played == 1) %>% 
    group_by(season, round, jumper_no, home_team, away_team) %>% 
    summarise(n = n(), .groups = 'drop') %>% View()

afltables %>% 
    select(id, first_name, surname, jumper_no, playing_for, season, date) %>% 
    distinct() %>% 
    group_by(id, jumper_no) %>% 
    filter(date == min(date)) %>% View

afltables %>% 
    select(season, round, date, id, first_name, surname, playing_for, home_team, away_team, home_score, away_score, kicks, handballs) %>% 
    mutate(
        disposals = kicks + handballs,
        team_score = case_when(
            playing_for == home_team ~ home_score,
            playing_for == away_team ~ away_score
        )
    ) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(total_games = row_number()) %>% 
    filter(disposals > total_games) %>% View()

afltables %>% 
    select(season, round, date, id, first_name, surname, disposals) %>% 
    filter(season == 2020) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(
        prev_15_19 = lag(disposals) >= 15 & lag(disposals) <= 19,
        current_15 = disposals >= 15
    ) %>% ungroup() %>% 
    filter(prev_15_19 == T) %>% 
    group_by(current_15) %>% 
    count() %>% ungroup() %>% 
    mutate(
        pcnt = n/sum(n)*100
    )
