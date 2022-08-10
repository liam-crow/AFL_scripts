
library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
library(stringr)
source("load_afltables.R")
source("load_fryzigg.R")

afltables %>% 
    select(season, id, first_name, surname, playing_for, round, w_l) %>% 
    filter(season > 2004, playing_for == 'Essendon', !(round %in% 1:25)) %>% View()

afltables %>% 
    select(season, round, playing_for, pq_1_g, pq_2_g, pq_3_g, pq_4_g, w_l) %>% 
    distinct() %>% 
    filter(!(round %in% 1:25)) %>% 
    mutate(
        final_half_goaless = pq_2_g == pq_3_g & pq_3_g == pq_4_g
    ) %>% View()

fryzigg_data %>% 
    filter(season == 2021) %>% 
    select(id, first_name, surname, fantasy_points,time_on_ground_percentage) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        fp = mean(fantasy_points),
        tog= mean(time_on_ground_percentage),
        n = n()
    ) %>% View()

height_data <- googlesheets4::read_sheet(ss = '1p03Mvo9TkB8pPhadl5fVHDYwBMrM_D5p9f6Cf7q60MM')

comb <- height_data %>% 
    mutate(comb = paste0(diff,'cm: ',Name1,' ',Height1,', ',Name2,' ',Height2,' (',Team,', ',years,')')) %>% 
    pull(comb)

paste0(comb, collapse = '')

afltables %>% 
    group_by(id, first_name, surname, jumper_no) %>% 
    summarise(games_played = n()) %>% View()

afltables %>% 
    select(playing_for, round, venue) %>% 
    distinct() %>% 
    filter(round == 'GF') %>% 
    group_by(playing_for) %>% 
    summarise(
        venues = paste0(unique(venue), collapse = ', '),
        n = n()
    ) %>% View()

n = 11

players_name <- afltables %>% 
    select(date, home_team, away_team, id, jumper_no, first_name, surname) %>% 
    distinct() %>% 
    filter(jumper_no != 0) %>% 
    mutate(
        fullname = paste0(first_name, surname) %>% str_to_lower() %>% substr(0,n),
        char_len = nchar(fullname)
    ) %>% 
    filter(char_len == n)

players_name %>% 
    inner_join(players_name, by = c('date', 'home_team', 'away_team', 'jumper_no', 'fullname', 'char_len')) %>% 
    filter(
        id.x != id.y,
        # first_name.x != first_name.y
    ) %>% 
    arrange(desc(date))

afltables %>% 
    select(id, first_name, surname, season, date, goals, behinds) %>% 
    mutate(total = goals*6 + behinds) %>% 
    filter(total != 0) %>% 
    mutate(
        year  = year(date) %>% substr(3,4) %>% as.numeric(),
        month = month(date),
        day   = day(date)
    ) %>% 
    filter(
        goals == day,
        behinds == month,
        total == year
    ) %>% View()

dates_scores <- tibble(
    date = seq.Date(from = as.Date('2000-01-01'),to = as.Date('2100-01-01'), by = 'day')
) %>% 
    mutate(
        year  = year(date) %>% substr(3,4) %>% as.numeric(),
        month = month(date),
        day   = day(date)
    ) %>% 
    filter(
        year == day*6 + month
    )

write.csv(dates_scores, "final_csv/dates_scores.csv", row.names = F)

afltables %>% 
    filter(season > 1964) %>% 
    select(season, round, date, playing_for, first_name, surname, disposals) %>% 
    group_by(season, round, date, playing_for, first_name) %>% 
    summarise(
        n = n(),
        t_disp = sum(disposals),
        surnames = paste0(surname, collapse = ', '),
        .groups = 'drop'
    ) %>% 
    group_by(n) %>% 
    filter(t_disp == max(t_disp)) %>% 
    arrange(-n)

afltables %>% 
    filter(season > 1964) %>% 
    select(season, round, date, home_team, away_team, first_name, surname, disposals) %>% 
    group_by(season, round, date, home_team, away_team, first_name) %>% 
    summarise(
        n = n(),
        t_disp = sum(disposals),
        surnames = paste0(surname, collapse = ', '),
        .groups = 'drop'
    ) %>% 
    group_by(n) %>% 
    filter(t_disp == max(t_disp), t_disp != 0) %>% 
    arrange(-n) %>% 
    select(-date)

afltables %>% 
    select(season, round, date, playing_for, opp, first_name, surname, disposals) %>% 
    filter(grepl('-', surname)) %>% 
    group_by(season, round, date, playing_for, opp) %>% 
    summarise(
        n = n(),
        t_disp = sum(disposals),
        comb = paste0(first_name,' ', surname, collapse = ', ')
    ) %>% 
    filter(n == 2) %>% 
    arrange(-n) %>% group_by(comb) %>% count() %>% arrange(-n)

afltables %>% 
    select(season, round, date, home_team, away_team, first_name, surname) %>% 
    filter(grepl('-', surname)) %>% 
    group_by(season, round, date, home_team, away_team) %>% 
    summarise(
        n = n(),
        comb = paste0(first_name,' ', surname, collapse = ', ')
    ) %>% 
    arrange(-n) %>% View()

afltables %>% 
    select(
        season, round, date, playing_for_short, id, first_name, surname,
        goals, behinds, disposals, kicks, handballs, tackles, marks, fantasy_points, games_played, jumper_no
    ) %>% 
    filter(jumper_no == 37, games_played == 1) %>% View()

jn_gf <- afltables %>% 
    filter(round == 'GF', season >= 1965) %>% 
    select(season, round, w_l, playing_for_short, id, first_name, surname, jumper_no) %>% 
    group_by(jumper_no, w_l) %>% count() %>% ungroup() %>% 
    pivot_wider(names_from = w_l, values_from = n, values_fill = 0) %>% 
    select(jumper_no,W,D,L) %>% 
    mutate(win_ratio = round(W/(W+L+D)*100,2))

write.csv(jn_gf, file = 'jn_gf.csv')    

afltables %>% 
    filter(season > 1970) %>% 
    select(season, goals, behinds) %>% 
    group_by(season) %>% 
    summarise(
        t_goals = sum(goals),
        t_behinds = sum(behinds)
    ) %>% 
    mutate(ratio = t_goals/(t_behinds+t_goals)) %>% View()

afltables %>% 
    select(id, first_name, surname, goals) %>% 
    group_by(id, first_name, surname, goals) %>% count() %>%
    group_by(goals) %>% 
    filter(n == max(n)) %>% 
    group_by(goals, n) %>% 
    summarise(
        names = paste(first_name, surname, collapse = ', ')
    ) %>% View()

afltables %>% 
    filter(goals != 0) %>% 
    select(season, round, playing_for_short, opp_short, goals) %>% 
    group_by(season, round, playing_for_short, opp_short) %>% 
    mutate(
        max_goals = max(goals),
        n = n()
    )

afltables %>% 
    filter(goals > 0) %>% 
    select(season, round, date, playing_for_short, opp_short, playing_for_score, opp_score, id, first_name, surname, goals) %>% 
    mutate(margin = playing_for_score - opp_score) %>% 
    group_by(season, round, date, playing_for_short, opp_short, playing_for_score, opp_score, margin) %>% 
    summarise(
        surnames = paste0(surname,' (',goals,')', collapse = ', '),
        n = n()
    ) %>% View()

april_fools_bl <- afltables %>% 
    select(season, date,playing_for_short,opp_short, playing_for_score, opp_score, w_l, id, first_name, surname, fantasy_points, goals,disposals, brownlow_votes) %>% 
    filter(
        season > 1980,
        month(date) == 4,
        day(date) == 1
    ) %>% 
    arrange(-disposals)

write.csv(april_fools_bl, 'april_fools_bl.csv', row.names = F)    
parallel::detectCores()

no <- 8

afltables %>% 
    filter(season > 1990, games_played %in% c(1:no)) %>% 
    select(id, first_name, surname, tackles, games_played) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(st = sum(tackles), n = n()) %>% filter(n == no) %>% 
    arrange(st)


afltables %>% 
    select(id, first_name, surname, playing_for_short, round) %>% 
    filter(round == 'GF') %>% 
    group_by(id, first_name, surname) %>%
    summarise(
        n = n(),
        teams = paste0(unique(playing_for_short),collapse = ', '),
        no_teams = length(unique(playing_for_short))
    ) %>% View()
