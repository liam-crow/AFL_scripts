
library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
source("load_afltables.R")
source("load_fryzigg.R")

afltables %>% 
    select(season, date, playing_for,pq_1_g,pq_1_b,pq_2_g,pq_2_b,pq_3_g,pq_3_b,pq_4_g,pq_4_b) %>% 
    distinct() %>% 
    mutate(
        pq_1_s = pq_1_g*6 + pq_1_b,
        pq_2_s = pq_2_g*6 + pq_2_b,
        pq_3_s = pq_3_g*6 + pq_3_b,
        pq_4_s = pq_4_g*6 + pq_4_b,
        .keep = 'unused'
    ) %>% 
    group_by(season, playing_for) %>% 
    summarise(
        total_ducks = sum(pq_1_s == 0) + sum(pq_2_s == pq_1_s) + sum(pq_3_s == pq_2_s) + sum(pq_4_s == pq_3_s)
    ) %>% View()

afltables %>% 
    select(season, id, first_name, surname, stat = disposals) %>% 
    filter(stat == 0, season > 1990) %>% 
    group_by(id, first_name, surname) %>% count() %>% View()

gk_data <- afltables %>% 
    select(
        season, round, date, playing_for, opp, home_team, away_team, 
        id, first_name, surname, goals, behinds, games_played
    ) %>% 
    # filter(season > 1960) %>% 
    group_by(id) %>% 
    mutate(
        goals_kicked = cumsum(goals),
        goals_prev   = lag(goals_kicked, default = 0),
        bh_kicked = cumsum(behinds),
        bh_prev   = lag(bh_kicked, default = 0)
    ) %>% ungroup() 

res <- NULL

for (i in 1:100) {
    iter <- gk_data %>% 
        filter(
            # (goals_kicked > 0 & goals_prev == 0)|
            games_played == i & (goals_kicked >= i & goals_prev < i) & (bh_kicked >= i & bh_prev < i)
        )
    res <- rbind(res,iter)
}

afltables %>% 
    select(season, round, playing_for, jumper_no, id, first_name, surname, disposals, bounces) %>% 
    filter(disposals<bounces) %>% View()

int_seq <- seq(1,100)

afltables %>% 
    select(id, first_name, surname, jumper_no_raw, jumper_no) %>% 
    filter(jumper_no != 0) %>% 
    group_by(jumper_no_raw, jumper_no) %>% count() %>% View()

afltables %>% 
    select(season, id, first_name, surname, jumper_no_raw, jumper_no) %>% 
    filter(grepl('↓|↑',jumper_no_raw)) %>% 
    group_by(id, first_name, surname,jumper_no_raw) %>% count() %>% View()
# ↓↑

afltables %>% 
    select(season, round, playing_for, opp, id, first_name, surname, marks, marks_inside_50, goals,goal_assists, behinds) %>% 
    filter(
        season >= 1999,
        # marks_inside_50 == 0,
        goals == 0,
    ) %>% 
    # arrange(-goals,-marks)
    arrange(-marks_inside_50) %>% View()

afltables %>% 
    filter(season > 1970) %>% 
    select(id, first_name, surname) %>% distinct() %>% 
    filter(nchar(surname) == 5) %>% View()

afltables %>% 
    select(season, round, venue, playing_for, h_a) %>% 
    distinct() %>% 
    filter(season > 2018, round %in% 1:25, playing_for %in% c('West Coast','Fremantle')) %>% 
    mutate(venue = if_else(venue == 'Perth Stadium','Perth','Not Perth')) %>% 
    group_by(season, venue) %>% 
    count()

afltables %>% 
    select(date, playing_for, id, first_name, surname, venue, round, w_l) %>% 
    filter(venue == 'Kardinia Park') %>% 
    View()

afltables %>% 
    select(season,id, first_name, surname, fantasy_points) %>% 
    filter(season > 1980) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        max_fp = max(fantasy_points),
        avg_fp = mean(fantasy_points),
        n = n()
    ) %>% 
    filter(max_fp >= 100) %>% 
    View()

afltables %>% 
    select(season, round, playing_for, opp, pq_2_g, pq_2_b, oq_2_g, oq_2_b, w_l,playing_for_score, opp_score) %>% 
    mutate(
        htp = pq_2_g*6 + pq_2_b,
        hto = oq_2_g*6 + oq_2_b
    ) %>% 
    distinct() %>% 
    View()

afltables %>% 
    select(date,playing_for,round,w_l) %>% 
    distinct() %>% 
    filter(!(round %in% 1:25),w_l == 'W') %>% 
    group_by(playing_for) %>% 
    mutate(diff = date - lag(date)) %>% View()
