
source("load_afltables.R")
source("fryzigg_data.R")
library(dplyr)
library(lubridate)

afltables %>% 
    select(date, id, first_name, surname, jumper_no, playing_for, opp,playing_for_score, opp_score) %>% 
    group_by(id) %>% 
    arrange(date) %>% 
    mutate(
        games_played = row_number(),
        margin = playing_for_score - opp_score
    ) %>% ungroup() %>% 
    filter(
        margin == jumper_no,
        games_played == 1,
        jumper_no > 0,
        day(date) == jumper_no
    )

afltables %>% 
    filter(season >= 1965) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        total_disp = sum(disposals),
        total_score= sum(goals*6 + behinds)
    ) %>% View()

afltables %>% 
    select(season, round, home_team, away_team, hq_1_g, hq_1_b, aq_1_g, aq_1_b) %>% distinct() %>% 
    filter(hq_1_g == 0 & aq_1_g == 0) %>% 
    mutate(score = hq_1_b + aq_1_b) %>% View()

afltables %>% 
    select(season, round, venue, home_team, away_team) %>% distinct() %>% View()

afltables %>% 
    filter(season >= 2000) %>% 
    select(id, first_name, surname, brownlow_votes) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        n = n(),
        sb= sum(brownlow_votes, na.rm = T)
    ) %>% View()

afltables %>% 
    select(playing_for, opp, venue) %>% distinct() %>% 
    group_by(playing_for, opp) %>% 
    count() %>% View()

afltables %>% 
    filter(date >= 1965) %>% 
    select(id, first_name, surname, goals, behinds) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        sg = sum(goals),
        sb = sum(behinds)
    ) %>% 
    filter(sg >= 100) %>% 
    mutate(
        ratio = round(sg/(sg+sb), 4) *100
    ) %>% View()

afltables %>% 
    select(season, round, date, playing_for, pq_4_g, pq_4_b, playing_for_score) %>% distinct() %>% 
    group_by(season, playing_for, pq_4_g, pq_4_b, playing_for_score) %>% count() %>% View()

afltables %>% 
    select(season, round, date, home_team, away_team, playing_for, jumper_no, id, first_name, surname) %>% 
    filter(grepl('[-]', surname)) %>% 
    group_by(season, round, date, home_team, away_team, jumper_no) %>%
    mutate(n = n()) %>% View()
    
afltables %>% 
    filter(
        grepl("^G", first_name),
        grepl("^F", surname)
    ) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(n = n()) %>% View()
