
library(dplyr)
library(tidyr)
source("load_afltables.R")
source("fryzigg_data.R")

# 1 point finals losses ####

afltables %>% 
    select(season, round, date, playing_for, playing_for_score, opp, opp_score) %>% distinct() %>% 
    filter(!(round %in% 1:30), season > 2000) %>% 
    mutate(margin = playing_for_score - opp_score) %>% 
    filter(between(margin,-1,-1)) %>% 
    group_by(playing_for, opp) %>% 
    summarise(
        n = n(),
        seasons = paste(season, round, playing_for_score, opp_score, collapse = ', ')
    ) %>% View()

afltables %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        max_no = max(jumper_no),
        min_no = min(jumper_no),
        diff = max_no-min_no
    ) %>% View()

afltables %>% 
    select(season, round, date, playing_for, starts_with('pq'), playing_for_score) %>% distinct() %>% 
    filter(!round %in% 1:30) %>% 
    mutate(ht_score = pq_2_g *6 + pq_2_b) %>% View()

afltables %>% 
    select(season, round, date, id, first_name, surname, playing_for, playing_for_score, opp_score) %>% distinct() %>% 
    filter(!round %in% 1:30) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        pcnt = sum(playing_for_score)/sum(opp_score),
        games = n()
    ) %>% View()

afltables %>% 
    select(season, round, date, id, first_name, surname, w_l) %>% distinct() %>% 
    filter(round == 'QF') %>% 
    group_by(id, first_name, surname, w_l) %>% 
    summarise(n = n(), .groups = 'drop') %>% 
    pivot_wider(
        names_from = w_l,
        values_from = n,
        values_fill = 0
    ) %>% 
    mutate(n = W+L+D) %>% View()

afltables %>% select(id, first_name, surname, playing_for, w_l) %>% 
    group_by(playing_for, id, first_name, surname, w_l) %>% 
    summarise(n = n(), .groups = 'drop') %>% 
    pivot_wider(names_from = w_l, values_from = n, values_fill = 0) %>% 
    group_by(id, first_name, surname) %>% 
    mutate(n = n(), games_played = L + D + W, s_win = sum(W), s_loss = sum(L), s_draw = sum(D)) %>% 
    filter(n == 3) %>%
    summarise(
        record = paste(games_played, collapse = '-'),
        s_win = unique(s_win),
        s_loss = unique(s_loss),
        s_draw = unique(s_draw),
        .groups = 'drop'
    ) %>% 
    mutate(
        c1 = paste(s_win, s_loss, s_draw, sep = '-'),
        c2 = paste(s_win, s_draw, s_loss, sep = '-'),
        c3 = paste(s_loss, s_win, s_draw, sep = '-'),
        c4 = paste(s_loss, s_draw, s_win, sep = '-'),
        c5 = paste(s_draw, s_win, s_loss, sep = '-'),
        c6 = paste(s_draw, s_loss, s_win, sep = '-'),
    ) %>%
    filter(record == c1 | record == c2 | record == c3 | record == c4 | record == c5 | record == c6) %>% View()

scoring_data <- afltables %>% 
    select(season, round, date, home_team, away_team, home_score, away_score) %>% 
    group_by(season, round, date, home_team, away_team) %>% 
    summarise(
        win_score = max(home_score, away_score),
        loss_score = min(home_score, away_score),
        .groups = 'drop'
    )

scoring_data %>% group_by(win_score, loss_score) %>% 
    arrange(date) %>% mutate(n = row_number()) %>% filter(n == 1) %>% View()

scorigami_by_year <- scoring_data %>% 
    group_by(win_score, loss_score) %>% 
    summarise(earliest_date = min(date)) %>% 
    group_by(year = year(earliest_date)) %>% count() 

library(ggplot2)
library(patchwork)

games_year <- scoring_data %>% group_by(year = year(date)) %>% 
    count(name = 'games')

scorigami <- full_join(scorigami_by_year, games_year, by = 'year') %>% 
    mutate(ratio = n/games * 100)

g1 <- ggplot(scorigami_by_year, aes(x = year, y = n)) +
    geom_point() + geom_smooth(span = 0.7) +
    xlab('') + ylab('Total Scorigami') +
    scale_y_continuous(limits=c(0, 100)) +
    ggtitle('Scorigami in the V/AFL', subtitle = "Useless AFL Stats, @crow_data_sci")

g2 <- ggplot(scorigami, aes(x = year, y = ratio)) +
    geom_point() + geom_smooth(span = 0.5) +
    xlab('Season') + ylab('% Scorigami') + 
    scale_y_continuous(limits=c(0, 100))

g1/g2

afltables %>% 
    select(date, id, first_name, surname, jumper_no, round, w_l) %>% 
    filter(w_l == 'W', round == 'GF') %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        n = n(),
        n_unique = length(unique(jumper_no)),
        n_all = paste(jumper_no, collapse = ', '),
        min_ = min(date)
    ) %>% View()

tips_round <- get_squiggle_data("tips", year = 2020) %>% as_tibble()
View(tips_round)

afltables %>% 
    group_by(id, first_name, surname, jumper_no) %>% 
    summarise(
        debut = min(date),
        games = n(),
        .groups = 'drop'
    ) %>% 
    filter(debut > '1964-01-01') %>% 
    mutate(
        power = log(games)/log(jumper_no),
        power_int = as.integer(power)
    ) %>% 
    filter(power == power_int, jumper_no != 0) %>% 
    arrange(-games, -jumper_no) %>% 
    mutate(comb = paste0(first_name,' ',surname,' #',jumper_no,', ',games,' games = ',jumper_no,'^',power)) %>% View()
    # group_by(id, first_name, surname) %>% count() %>% 
    pull(comb) %>% paste(collapse = ', ')

afltables %>% 
    select(id, first_name, surname, date, jumper_no) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(
        games_played = row_number(),
        debut = min(date)
    ) %>% 
    filter(
        # debut > '1964-01-01',
        games_played == jumper_no
    ) %>% 
    mutate(n = n()) %>% View()
    