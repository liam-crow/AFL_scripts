
library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
source("load_afltables.R")
source("load_fryzigg.R")

afltables %>% 
    select(playing_for_short, id, first_name, surname, jumper_no, season) %>% 
    filter(jumper_no > 0) %>% 
    distinct() %>% 
    group_by(playing_for_short, id, first_name, surname, jumper_no) %>% 
    count() %>% 
    filter(jumper_no == n) %>% View()

afltables %>% 
    select(season, round, home_team, away_team, hq_1_g, hq_1_b, aq_1_g,aq_1_b) %>% distinct() %>% View()

afltables %>% 
    select(season, date, round, home_team, away_team, playing_for, w_l) %>% 
    distinct() %>% View()

afltables %>% 
    select(season, round, home_team, away_team, date, playing_for, 
           id, first_name, surname, jumper_no, fantasy_points) %>% 
    filter(fantasy_points != 0, jumper_no != 0, season > 2000) %>% 
    group_by(season, round, date, home_team, away_team, fantasy_points) %>% 
    mutate(
        n = n(),
        t_jn = sum(jumper_no)
    ) %>% 
    filter(t_jn == fantasy_points) %>% View()

afltables %>% 
    select(season, round, venue, playing_for, opp, 
           starts_with('pq'), starts_with('oq')) %>% 
    distinct() %>% 
    filter(season >= 2010) %>% 
    mutate(
        pq1 = pq_1_g*6+pq_1_b,
        pq2 = pq_2_g*6+pq_2_b,
        pq3 = pq_3_g*6+pq_3_b,
        pq4 = pq_4_g*6+pq_4_b,
        oq1 = oq_1_g*6+oq_1_b,
        oq2 = oq_2_g*6+oq_2_b,
        oq3 = oq_3_g*6+oq_3_b,
        oq4 = oq_4_g*6+oq_4_b,
        .keep = "unused"
    ) %>% 
    mutate(
        lead_at_all_4 = pq1>oq1,pq2>oq2,pq3>oq3,pq4>oq4,
    ) %>%
    group_by(playing_for, lead_at_all_4) %>% count() %>% 
    group_by(playing_for) %>% 
    mutate(pcnt = 100*(n/sum(n))) %>% View()
