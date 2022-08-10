library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
source("load_afltables.R")
source("load_fryzigg.R")

fryzigg_data %>% 
    select(season, date, round, id, first_name, surname, fantasy_points, afl_fantasy_score) %>% 
    filter(fantasy_points != afl_fantasy_score)

afltables %>% 
    select(season, round, date, playing_for, opp, playing_for_score, opp_score) %>% 
    distinct() %>% 
    filter(playing_for_score == 2*opp_score) %>% 
    View()

fryzigg_data %>% 
    select(season, date, round, id, first_name, surname, ruck_contests, hit_outs, hitout_win_percentage, hitouts_to_advantage) %>% 
    View()

afltables %>% 
    select(season, date, round, home_team, away_team, hq_1_g, aq_1_g) %>% 
    distinct() %>% 
    mutate(tq1 = hq_1_g + aq_1_g) %>% View()

afltables %>% 
    select(season, date, round, playing_for, home_team, away_team, id, first_name, surname, jumper_no, disposals) %>% 
    filter(jumper_no == disposals, disposals>0) %>% 
    group_by(season, date, round, home_team, away_team) %>% 
    summarise(
        n = n(),
        names = paste0(surname, ' #', jumper_no, collapse = ', ')
    ) %>% View()

afltables %>% 
    select(season, date, round, playing_for, id, first_name, surname) %>% 
    filter(grepl("^loch|^lach|^lauch",first_name, ignore.case = T)) %>% 
    group_by(season, date, round, playing_for) %>% 
    summarise(
        n = n(),
        names = paste(first_name, surname, collapse = ', ')
    ) %>% View()

afltables %>% 
    select(season, playing_for, id, first_name, surname) %>% 
    filter(
        season > 2010,
        # grepl("^loch|^lach|^lauch|^lac",first_name, ignore.case = T)
        grepl("loc",surname, ignore.case = T)
    ) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        n = n(),
        names = paste(unique(playing_for), collapse = ', ')
    ) %>% View()

afltables %>% 
    select(season, round, date, id, first_name, surname, playing_for_short, first_name, goals) %>% 
    filter(goals >= 7) %>% 
    group_by(date, round, season) %>% 
    summarise(
        n = n(),
        names = paste(first_name, surname, playing_for_short, goals, collapse = ', ')
    ) %>% View()

fryzigg_data %>% 
    filter(season == 2021) %>% 
    select(guernsey_number, turnovers) %>% 
    group_by(guernsey_number) %>% count() %>% View()

fryzigg_data %>% 
    select(season, round, date, playing_for, id, first_name, surname, marks, marks_on_lead, contested_marks) %>% 
    mutate(cheap_marks = marks - marks_on_lead - contested_marks) %>% 
    View()
afltables %>% 
    select(season, round, date, playing_for, opp, id, first_name, surname, frees_against) %>% View()

#### worst win % with GF win ####

afltables %>% 
    select(season, date, round, id, first_name, surname, w_l, games_played) %>% 
    group_by(id, first_name, surname) %>% 
    filter(any(round == 'GF' & w_l == 'W')) %>% 
    mutate(
        total_games = max(games_played)
    ) %>% 
    filter(w_l == 'W') %>% 
    group_by(id, first_name, surname, total_games) %>% 
    count() %>% 
    mutate(win_rate = round(n/total_games*100,2)) %>% View()

afltables %>% 
    select(season, date, round, id, first_name, surname, w_l, games_played) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        gf_wins = sum(round == 'GF' & w_l == 'W'),
        wins = sum(w_l == 'W'),
        total_games = max(games_played),
        win_rate = round(wins/total_games*100,2), .groups = 'drop'
    ) %>% 
    filter(gf_wins > 0) %>% 
    group_by(gf_wins) %>% 
    arrange(win_rate) %>% 
    slice(1:5) %>% View()

afltables %>% 
    select(season, id, first_name, surname, playing_for, opp, games_played) %>% 
    group_by(id, first_name, surname) %>% 
    filter(games_played == min(games_played) | games_played == max(games_played)) %>% 
    mutate(
        first_last = case_when(
            row_number() == 1 ~ 'debut',
            row_number() == 2 ~ 'final',
            T ~ 'other'
        ) 
    ) %>%
    ungroup() %>% 
    select(-games_played) %>% 
    pivot_wider(
        names_from = first_last,
        values_from = c(season, playing_for, opp)
    ) %>% 
    filter(playing_for_debut == opp_final & playing_for_final == opp_debut) %>% 
    select(first_name, surname, season_debut, playing_for_debut, opp_debut, season_final, playing_for_final, opp_final) %>% View()
    
fryzigg_data %>% 
    select(season, round, playing_for, id, first_name, surname, brownlow_votes, time_on_ground_percentage, subbed, kicks, frees_for, frees_against, handballs, disposals, bounces, goals, behinds, contested_possessions, contested_marks, marks, marks_inside_fifty, fantasy_points, supercoach_score) %>% 
    filter(season != 2021, brownlow_votes != 0) %>% View()

fryzigg_data %>% 
    select(season, playing_for, id, first_name, surname, brownlow_votes) %>% 
    group_by(season, playing_for, id, first_name, surname) %>% 
    summarise(t_bv = sum(brownlow_votes),.groups = 'keep') %>% 
    group_by(playing_for, id, first_name, surname) %>% 
    filter(season == max(season), season != 2021) %>% View()

afltables %>% 
    select(season, home_team, away_team, round, id, first_name, surname, kicks, handballs, goals, brownlow_votes) %>% 
    filter(brownlow_votes != 0) %>% 
    group_by(season, round, home_team, away_team, kicks, goals) %>% 
    filter(sum(brownlow_votes) == 6) %>% View()

afltables %>% 
    select(season, home_team, away_team, round, id, first_name, surname, brownlow_votes) %>% 
    filter(brownlow_votes != 0) %>% 
    group_by(season, round, home_team, away_team, first_name) %>% 
    filter(sum(brownlow_votes) == 6) %>% View()

afltables %>% 
    select(season, date,round,playing_for, id, first_name, surname, goals, behinds, venue) %>% 
    filter(goals == 5, behinds == 8) %>% View()

afltables %>% 
    select(date, season, round, playing_for_short, id, first_name, surname, w_l) %>% 
    filter(season > 1990, round %in% c('PF','GF')) %>% 
    group_by(season, playing_for_short) %>% 
    mutate(team_games = length(unique(round))) %>% ungroup() %>% 
    group_by(season, id, first_name, surname) %>% 
    mutate(player_games = length(unique(round))) %>% ungroup() %>% 
    filter(team_games == 2, player_games == 1, round == 'PF') %>% View()

fryzigg_data %>% 
    select(season, date, season, round, playing_for, id, first_name, surname, position, home_team, away_team, home_team_score, away_team_score) %>%
    mutate(
        w_l = case_when(
            playing_for == home_team & home_team_score > away_team_score ~ 'W',
            playing_for == home_team & home_team_score < away_team_score ~ 'L',
            playing_for == away_team & home_team_score < away_team_score ~ 'W',
            playing_for == away_team & home_team_score > away_team_score ~ 'L',
            T ~ 'D'
        )
    ) %>% 
    filter(round == "Grand Final", w_l == 'W') %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        n = n(),
        pos = paste(season, position, collapse = ', ')
    ) %>% View()

afltables %>% 
    select(season, date, round, playing_for, w_l) %>% distinct() %>% 
    mutate(year_lt = gsub('^\\w{2}','', season)) %>% 
    filter(grepl('(.)\\1', year_lt)) %>% 
    group_by(playing_for, w_l) %>% 
    summarise(n = n()) %>% 
    pivot_wider(
        names_from = w_l,
        values_from = n,
        values_fill = 0
    ) %>% 
    mutate(winrate = round(W/(W+L+D)*100,2)) %>% View()
    
afltables %>% 
    select(season, id, first_name, surname, opp, brownlow_votes) %>% 
    filter(brownlow_votes == 3) %>%
    group_by(id, first_name, surname, opp) %>% 
    summarise(
        t_votes = sum(brownlow_votes)/3
    ) %>% View()

afltables %>% 
    select(season, round, id, first_name, surname, possessions, brownlow_votes) %>% 
    filter(season > 1983, round %in% 1:25) %>% 
    group_by(season, id, first_name, surname) %>% 
    summarise(
        n = n(),
        t_bl = sum(brownlow_votes),
        avg_pos = mean(possessions)
    ) %>% 
    group_by(season) %>% 
    # arrange(desc(t_bl)) %>% 
    slice_max(n = 2, order_by = t_bl) %>% View()

afltables %>% 
    select(season, playing_for_short, id, first_name, surname, round, w_l) %>% 
    filter(round == 'GF') %>% 
    group_by(id, first_name, surname) %>% 
    filter(!any(w_l == 'W')) %>% 
    summarise(
        losing_gf = n(),
        unique_teams = length(unique(playing_for_short)),
        comb = paste(season, playing_for_short, collapse = ', '),
        recent = max(season),
        .groups = 'drop'
    ) %>%
    arrange(-recent) %>% 
    filter(unique_teams >= 2) %>% 
    summarise(
        final = paste0(first_name,' ',surname,' (',comb,')', collapse = '\n')
    ) %>% View()
    
afltables %>% 
    filter(season > 2019) %>% 
    select(playing_for, round, id, first_name, surname, brownlow_votes) %>% View()

fryzigg_data %>% 
    select(playing_for, id, first_name, surname, season, date, round, fantasy_points, weather_temp_c, weather_type) %>% 
    drop_na() %>%
    group_by(season) %>% count() %>% 
    View()

afltables %>% 
    select(playing_for, id, first_name, surname, round, w_l) %>% 
    distinct() %>% 
    filter(round == 'GF' & w_l == 'W') %>% 
    group_by(playing_for) %>% 
    summarise(
        n = n()
    ) %>% View()

fryzigg_data %>% 
    filter(season == 2021) %>% 
    select(playing_for, height_cm) %>% 
    group_by(playing_for) %>% 
    summarise(
        avg_h = mean(height_cm)
    ) %>% View()

fryzigg_data %>% 
    filter(season == 2021) %>% 
    select(season, round, home_team, away_team, playing_for, id, first_name, surname, height_cm) %>% 
    group_by(season, round, home_team, away_team) %>% 
    filter(height_cm == max(height_cm) | height_cm == min(height_cm)) %>% 
    mutate(diff = max(height_cm)-min(height_cm)) %>% View()

afltables %>% 
    select(season, round, playing_for_short, id, first_name, surname, frees_for, games_played) %>% 
    filter(games_played == 1) %>% View()

afltables %>% 
    select(season, round, id, first_name, surname) %>% 
    filter(grepl('',surname)) %>% View()

fryzigg_data %>% 
    filter(season %in% 2011:2021) %>% 
    select(id, first_name, surname, kicks, marks, handballs, disposals, goals, behinds, 
           hit_outs, spoils, rebounds, rating_points, fantasy_points) %>% 
    group_by(id, first_name, surname) %>% 
    filter(any(rebounds>6), any(spoils>6)) %>% 
    pivot_longer(
        cols = kicks:fantasy_points,
        names_to = 'stat_name'
    ) %>% 
    group_by(stat_name) %>%
    mutate(league_avg = mean(value, na.rm = T)) %>% 
    group_by(id, first_name, surname, stat_name, league_avg) %>% 
    summarise(
        games = n(),
        player_avg = mean(value, na.rm = T),
        player_sd  = sd(value),
        .groups = 'drop'
    ) %>% 
    filter(games > 25) %>% 
    mutate(diff_mean = abs(player_avg - league_avg)) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(total_diff = sum(diff_mean)) %>% View()

nrl_results <- read.csv("nrldata.csv") %>% as_tibble() %>% 
    mutate(date = dmy(date))

afl_results <- afltables %>% 
    select(year = season, date, round, home_team, away_team, home_score, away_score) %>% distinct() %>% 
    filter(year %in% c(2017:2021))

result_join <- inner_join(nrl_results, afl_results, by = c('year','date'))

result_join %>% 
    filter(home_score == hscore | away_score == ascore)

result_join %>% 
    filter(home_score == ascore | away_score == hscore)

result_join %>% 
    filter(home_score/hscore == away_score/ascore | away_score/hscore == home_score/ascore) %>% 
    mutate(ratio = home_score/hscore) %>% View()

afltables %>% 
    select(season, date, id, first_name, surname, jumper_no) %>% 
    group_by(id, first_name, surname) %>% 
    filter(any(jumper_no == 1)) %>% 
    summarise(
        debut = min(season),
        games_played = n()
    ) %>% View()

afltables %>% 
    select(playing_for, id, first_name, surname, jumper_no) %>%
    filter(jumper_no == 9, playing_for == 'Port Adelaide') %>% distinct() %>% View()

afltables %>% 
    select(season, round, date, home_team, away_team, playing_for, id, first_name, surname, disposals) %>% 
    filter(season > 1980) %>% 
    group_by(season, round, date, home_team, away_team, playing_for) %>% 
    filter(!any(disposals %in% c(3,13,30:39,43) & disposals > 40)) %>% View()
    
