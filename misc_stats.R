
library(dplyr)
source("load_afltables.R")
library(ggplot2)
library(tidyr)
source("fryzigg_data.R")

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
    filter(date == min(date)) %>% View()

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

no_weight <- fryzigg_data %>% select(season, match_round, player_team, guernsey_number, player_height_cm) %>% 
    filter(season == 2020, match_round == 8)

ggplot(no_weight, aes(x = guernsey_number, y = player_height_cm)) + 
    geom_point() + geom_smooth()

afltables %>% 
    filter(season >= 1965) %>% 
    select(playing_for, starts_with('ump'), frees_for, frees_against) %>% 
    pivot_longer(
        cols = starts_with("ump"),
        names_to = 'umpire_id',
        values_to = 'umpire_name'
    ) %>% 
    mutate(umpire_name = gsub(' ','', umpire_name)) %>% 
    group_by(playing_for, umpire_name) %>% 
    summarise(
        s_ff = sum(frees_for),
        s_fa = sum(frees_against),
        .groups = 'keep'
    ) %>% 
    mutate(diff = s_ff - s_fa) %>% 
    View()

afltables %>% 
    select(season, round, playing_for, playing_for_score, opp, opp_score, 
           pq_4_g, pq_4_b, oq_4_g, oq_4_b) %>% distinct() %>% 
    mutate(
        p_shots = pq_4_g + pq_4_b, 
        o_shots = oq_4_g + oq_4_b,
        s_diff = p_shots - o_shots,
        margin = playing_for_score - opp_score,
        p_acc = pq_4_g/(pq_4_g + pq_4_b),
        o_acc = oq_4_g/(oq_4_g + oq_4_b),
        p_diff = pq_4_b - pq_4_g,
        a_diff = p_acc - o_acc
    ) %>% View()
filter(s_diff %in% -1:1) %>% View()

afltables %>% 
    select(season, round, date, home_team, away_team, id, first_name, surname) %>% 
    filter(grepl('wert', surname, ignore.case = T)) %>% 
    distinct(first_name, surname) %>% View()
group_by(season, round, date, home_team, away_team) %>% 
    summarise(
        n = n(),
        stat = paste(first_name, surname, collapse = ' & '),
        .groups = 'drop'
    ) %>% 
    mutate(
        stat2 = paste0(season, ' r', round, ' ', home_team, ' vs ', away_team, ' ', stat)
    ) %>% #filter(n==3) %>% 
    arrange(-n) %>% select(stat2) %>% View()
# Peter Burgoyne
# Shaun Burgoyne
# Cliff Burge
# Fred Burge
# Reg Burgess
# Ted Burgess
# Graham Burgin
# Lloyd Burgmann
# Tony Burgess
# Terry Burgess
# Greg Burgess
# Shane Burgmann
# Jack Burgmann
# Chris Burgess


fryzigg_data %>% 
    select(season, player_team, player_first_name, player_last_name, player_height_cm, hitouts, hitouts_to_advantage, hitout_win_percentage) %>% 
    group_by(season, player_team, player_first_name, player_last_name, player_height_cm) %>% 
    summarise(t_ho=sum(hitouts)) %>% 
    View()

afltables %>% 
    distinct(season, round, date, home_team, away_team, id, first_name, surname) %>% 
    pivot_longer(
        col = c(first_name, surname),
        names_to = 'cat',
        values_to = "names"
    ) %>% 
    mutate(names = trimws(tolower(names))) %>% 
    group_by(season, round, date, home_team, away_team, names) %>% 
    count() %>% View()

afltables %>% select(season, round, date, playing_for, playing_for_score, opp_score, w_l) %>% distinct() %>% 
    filter(w_l == 'W', round %in% 1:25, season > 1965) %>% 
    mutate(margin = playing_for_score - opp_score) %>% 
    group_by(season, round) %>% 
    summarise(
        s_win = sum(playing_for_score),
        m_win = mean(playing_for_score),
        s_mar = sum(margin),
        m_mar = mean(margin),
        .groups = 'drop'
    ) %>% View()

fryzigg_data %>% select(match_date, match_round, player_id, player_first_name, player_last_name, kicks, marks, intercept_marks, intercepts) %>% 
    drop_na() %>% View()

fryzigg_data %>% 
    select(match_date, match_round, player_id, player_first_name, player_last_name, 
           disposals, score_involvements, clangers) %>% 
    drop_na() %>% 
    filter(disposals == score_involvements + clangers) %>% 
    View()

afltables %>% 
    select(season, round, date, playing_for, playing_for_score) %>% distinct() %>% 
    group_by(season, round, playing_for_score) %>% 
    count() %>% View()

afltables %>% 
    group_by(id, first_name, surname) %>% arrange(date) %>% 
    mutate(games_played = row_number()) %>% 
    filter(games_played %in% 1:100) %>% 
    summarise(
        s_g = sum(goals),
        s_b = sum(behinds),
        date = max(date),
        games = max(games_played)
    ) %>% View()

library(dplyr)
library(lubridate)
afltables %>% 
    select(season, round, date, home_team, away_team, home_score, away_score, w_l) %>% distinct() %>% 
    mutate(day = weekdays(date)) %>% View()
# filter(weekdays(date) == 'Wednesday', w_l == 'D')

afltables %>% 
    select(season, round, date, home_team, away_team, id, first_name, surname, jumper_no, kicks, handballs) %>% 
    filter(jumper_no == kicks + handballs, jumper_no != 0) %>% 
    group_by(season, round, date, home_team, away_team) %>% 
    mutate(n = n()) %>% View()

afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname, goals) %>% 
    filter(goals > 0) %>% 
    mutate(
        last_char = str_sub(first_name, start = -2)
    ) %>% 
    group_by(season, round, date, playing_for) %>% 
    mutate(
        n = length(unique(last_char)),
        n_players = n(),
        sum_g = sum(goals)
    ) %>% ungroup() %>% View()

afltables %>% 
    select(date, id, first_name, surname) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(games_played = row_number()) %>% 
    filter(games_played %in% c(1,10)) %>% 
    mutate(diff = date - lag(date)) %>% View()

fryzigg_data %>% 
    select(season, match_round, player_id, player_first_name, player_last_name, guernsey_number,
           contested_possessions, uncontested_possessions, intercept_marks, intercepts, match_margin) %>% 
    mutate(possessions = contested_possessions + uncontested_possessions) %>% 
    filter(possessions == intercepts) %>%
    group_by(player_id, player_first_name, player_last_name) %>% 
    mutate(n = n()) %>% 
    View()

afltables %>% 
    select(
        season, round, playing_for, id, first_name, surname,
        hit_outs, goals, behinds) %>% 
    filter(hit_outs >= 10, goals == 0) %>% 
    arrange(-behinds)

afltables %>% 
    select(
        season, round, playing_for, id, first_name, surname,
        hit_outs, goals, behinds)

afltables %>% 
    filter(season>=2002) %>%
    select(season, round, date, playing_for, home_team, away_team, starts_with('ump'), frees_for, frees_against) %>% 
    group_by(season, round, date, umpire_1, umpire_2, umpire_3, umpire_4, playing_for, home_team, away_team) %>% 
    summarise(
        s_ff = sum(frees_for),
        s_fa = sum(frees_against),
        .groups = 'drop'
    ) %>% 
    pivot_longer(
        cols = starts_with("ump"),
        names_to = 'umpire_id',
        values_to = 'umpire_name'
    ) %>% drop_na() %>% 
    View()

afltables %>% 
    select(season, round, date, playing_for, playing_for_score, opp, opp_score, w_l) %>% 
    distinct() %>% 
    filter(month(date) == 8, day(date) == 15) %>% 
    View()

afltables %>% distinct(playing_for, id, first_name, surname) %>% View()


afltables %>% 
    filter(season >= 1999) %>% 
    select(date, id, first_name, surname, bounces) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(cs_b = cumsum(bounces), games = row_number()) %>% View()

afltables %>% 
    select(season, round, playing_for, opp, id, first_name, surname, goals, behinds, oq_4_g, oq_4_b, playing_for_score, opp_score) %>% 
    mutate(
        player_score = goals*6 + behinds,
        diff = player_score - opp_score
    ) %>% View()

afltables %>% 
    filter(season >= 2000) %>% 
    distinct(venue)

afltables %>% 
    select(date, id, first_name, surname, jumper_no, w_l) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(games_played = row_number()) %>% ungroup() %>% 
    group_by(games_played) %>% 
    mutate(
        n_unique = length(unique(w_l)),
        n = n()
    ) %>% 
    distinct(games_played,n) %>% View()

afltables %>% select(id, first_name, surname, 
                     contested_possessions, uncontested_possessions) %>% 
    mutate(diff = contested_possessions - uncontested_possessions) %>% 
    View()

afltables %>% 
    group_by(id, first_name, surname) %>% 
    summarise(gf_win = sum(if_else(round == 'GF' & w_l == 'W',1,0))) %>% View()

afltables %>% 
    select(season, round, playing_for, id, first_name, surname) %>% 
    group_by(season, round, playing_for) %>% 
    summarise(
        team = paste0(first_name, surname, sep = ' ', collapse = ', ')
    ) %>% 
    group_by(team) %>%
    mutate(n = n()) %>% View()

afltables %>% 
    select(season, date, venue) %>% distinct() %>% 
    group_by(season, venue) %>% count() %>% View()

fryzigg_data %>% 
    filter(season == 2020) %>% 
    select(season, match_round, match_home_team, match_away_team,
           player_team, player_height_cm) %>% 
    group_by(season, match_round, player_team) %>% 
    mutate(avg_height = mean(player_height_cm)) %>% View()

afltables %>% 
    select(id, first_name, surname, jumper_no) %>% 
    group_by(id, first_name, surname) %>% 
    mutate(
        n = n(),
        n_no = length(unique(jumper_no))
    ) %>% 
    filter(n == n_no) %>% View()

afltables %>% 
    filter(season >= 1965, goals > 0, disposals > 0) %>% 
    select(season, round, playing_for, id, first_name, surname, goals, disposals) %>% 
    group_by(season, round, playing_for, goals, disposals) %>% 
    mutate(
        n = n()
    ) %>% View()

afltables %>% 
    filter(season >= 2009) %>% 
    filter(
        # id == 11768, #Nic Nat
        playing_for == "West Coast"
    ) %>% 
    select(season, id, playing_for, first_name, surname, w_l) %>% 
    # group_by(id, first_name, surname, w_l) %>% 
    group_by(playing_for, w_l) %>%
    count() %>% 
    ungroup() %>% 
    mutate(wr = round(n/sum(n), 4)*100)


afl_games_letter <- afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname) %>% 
    mutate(
        first_name = tolower(first_name) %>% trimws(),
        surname = tolower(surname) %>% trimws(),
        first_last = paste0(first_name, surname)
    ) %>% 
    group_by(season, round, date, playing_for) %>% 
    summarise(
        letter = unlist(strsplit(first_last,'')),
        .groups = 'drop'
    )

afl_games_letter %>% 
    group_by(season, round, date, playing_for, letter) %>% 
    summarise(n = n()) %>% 
    group_by(season, round, date, playing_for) %>% 
    summarise(diff = max(n)-min(n)) %>% View()
