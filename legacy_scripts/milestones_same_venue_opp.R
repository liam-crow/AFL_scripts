
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

games <- afltables %>% 
    select(season, date, id, first_name, surname, venue, playing_for, home_team, home_score, away_team, away_score) %>% 
    mutate(
        playing_for_score = case_when(
            playing_for == home_team ~ home_score,
            TRUE ~ away_score
        ),
        opp = case_when(
            playing_for == home_team ~ away_team,
            TRUE ~ home_team
        ),
        opp_score = case_when(
            playing_for == home_team ~ away_score,
            TRUE ~ home_score
        ),
        h_a = case_when(
            playing_for == home_team ~ 'H',
            TRUE ~ 'A'
        ),
        w_l = case_when(
            playing_for == home_team & home_score > away_score ~ 'W',
            playing_for == home_team & home_score < away_score ~ 'L',
            playing_for == away_team & away_score > home_score ~ 'W',
            playing_for == away_team & away_score < home_score ~ 'L',
            TRUE ~ 'D'
        )
    ) %>% 
    select(-home_team,-home_score,-away_team,-away_score) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(
        game_no = row_number(),
        debut = min(date)
    )

games %>% 
    filter(
        all(c(150,200,250) %in% game_no),
        game_no %in% c(150,200,250) 
    ) %>% 
    mutate(
        ven_u = length(unique(venue)),
        opp_u = length(unique(opp))
    ) %>% 
    filter(
        ven_u == 1,
        opp_u == 1
    ) %>% 
    select(id, first_name, surname, venue, opp) %>% 
    distinct()
    View()

