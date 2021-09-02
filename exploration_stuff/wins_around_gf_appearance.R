library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)

source("load_afltables.R")
source("fryzigg_data.R")

winrate_around_GF <- afltables %>% 
    select(season, date, round, playing_for, w_l) %>% distinct() %>% 
    group_by(season, playing_for) %>% 
    summarise(
        wins = sum(w_l == 'W'),
        games = n(),
        gf_yn = any(round == 'GF'),
        gf_win = any(round == 'GF' & w_l == 'W'),
        .groups = 'drop'
    ) %>% 
    group_by(playing_for) %>% arrange(season) %>% 
    mutate(
        wins_last_two = lag(wins,2) + lag(wins,1),
        games_last_two = lag(games,2) + lag(games,1),
        wins_next_two = lead(wins,2) + lead(wins,1),
        games_next_two = lead(games,2) + lead(games,1),
        t_wins  = wins_last_two + wins_next_two,
        t_games = games_last_two + games_next_two,
        win_ratio = round(t_wins/t_games*100,2)
    ) %>% 
    filter(gf_yn == TRUE, season>1965) %>% drop_na()

winrate_after_GF <- afltables %>% 
    select(season, date, round, playing_for, w_l) %>% distinct() %>% 
    group_by(season, playing_for) %>% 
    summarise(
        wins = sum(w_l == 'W'),
        games = n(),
        gf_yn = any(round == 'GF'),
        gf_win = any(round == 'GF' & w_l == 'W'),
        .groups = 'drop'
    ) %>% 
    group_by(playing_for) %>% arrange(season) %>% 
    mutate(
        wins_next_two = lead(wins,4) + lead(wins,3) + lead(wins,2) + lead(wins,1),
        games_next_two = lead(games,4) + lead(games,3) + lead(games,2) + lead(games,1),
        win_ratio = round(wins_next_two/games_next_two*100,2)
    ) %>% 
    filter(gf_yn == TRUE) %>% drop_na()


library(ggplot2)
library(ggdark)

ggplot(winrate_around_GF, aes(x = season, y = win_ratio)) +
    geom_point() +
    ggtitle(
        'Winning percentage of every team to appear in an AFL Grand Final\nin the two years before and after the season of appearance',
        'Since 1965, @crow_data_sci'
    ) +
    xlab('Season') +
    ylab('Win Percentage') +
    annotate('text',x = 2012, y = 39, label = '2007 Port Adelaide') +
    dark_theme_classic()
