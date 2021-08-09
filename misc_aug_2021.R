library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
source("load_afltables.R")
source("fryzigg_data.R")

#Nick Beltrami

fryzigg_data %>% 
    select(season, date, id = player_id, first_name = player_first_name, surname = player_last_name, supercoach_score, fantasy_points) %>% 
    group_by(id, first_name, surname) %>% 
    filter(season == max(season)) %>% 
    summarise(
        season = unique(season),
        games = n(),
        avg_sc = mean(supercoach_score),
        avg_fp = mean(fantasy_points),
    ) %>% View()

# mayne 94 fantasy points

afltables %>% 
    select(id, first_name, surname) %>% distinct() %>% View()

# Bailley Baltrusaitis
# Hey, wondering if there's is a thing for a team 
# losing to the 2 Perth teams by the same amount in the same season?
# Richmond lost by 4 points to both this year.

afltables %>% 
    select(season, round, playing_for, opp, playing_for_score, opp_score) %>% 
    distinct() %>% 
    mutate(margin = playing_for_score - opp_score) %>% 
    filter(opp %in% c('West Coast', 'Fremantle')) %>% 
    group_by(season, playing_for, margin) %>% 
    summarise(
        n = n(),
        res = paste(opp,'r', round, collapse = ', ')
    ) %>% 
    View()

afltables %>% 
    select(season, round, playing_for, opp, playing_for_score, opp_score) %>% 
    distinct() %>% 
    mutate(margin = playing_for_score - opp_score) %>% 
    filter(opp %in% c('Brisbane Lions', 'Gold Coast')) %>% 
    group_by(season, playing_for, margin) %>% 
    summarise(
        n = n(),
        res = paste(opp,'r', round, collapse = ', ')
    ) %>% 
    View()

afltables %>% 
    select(season, round, playing_for, opp, playing_for_score, opp_score) %>% 
    distinct() %>% 
    mutate(margin = playing_for_score - opp_score) %>% 
    filter(opp %in% c('Port Adelaide', 'Adelaide')) %>% 
    group_by(season, playing_for, margin) %>% 
    summarise(
        n = n(),
        res = paste(opp,'r', round, collapse = ', ')
    ) %>% 
    View()

afltables %>% 
    select(season, round, playing_for, opp, playing_for_score, opp_score) %>% 
    distinct() %>% 
    mutate(margin = playing_for_score - opp_score) %>% 
    filter(opp %in% c('Greater Western Sydney', 'Sydney')) %>% 
    group_by(season, playing_for, margin) %>% 
    summarise(
        n = n(),
        res = paste(opp,'r', round, collapse = ', ')
    ) %>% 
    View()


# Cameron Jones
# Hey team, love your work! Just wondering if you have 
# a stat for the most a team has made up in the final quarter to still lose?
# So if a team was down by 60 and lost by 10, 
# they made up 50 but still lost.
# What’s the biggest margin to make up that 
# didn’t come away with the win

afltables %>% 
    select(season, round, playing_for, opp, pq_3_g, pq_3_b, oq_3_g, oq_3_b, playing_for_score, opp_score) %>% 
    mutate(
        margin = playing_for_score - opp_score,
        q3_margin = pq_3_g * 6 + pq_3_b - oq_3_g *6 - oq_3_b,
        margin_diff = margin - q3_margin
    ) %>% 
    filter(
        margin < 0,
        q3_margin < 0
    ) %>% 
    distinct() %>% View()

# Silas Weare

# Found the podcast last week boys and loving it on 
# the way too and from work, very useless indeed! 
# Just wondering if anyone’s explored the idea 
# of what the ladder would look like if the winning 
# team received their margin as points, e.g. Dees 
# getting 98 points after they beat the Suns. 
# Would we see any past premiers missing out on 
# finals if this was the case? Something to chew 
# on if you get bored, stay useless

afltables_custom <- afltables %>% 
    select(season, round, date, playing_for, playing_for_score, opp_score) %>% 
    mutate(
        w_l = case_when(
            playing_for_score > opp_score ~ 'W',
            playing_for_score < opp_score ~ 'L',
            T ~ 'D',
        )
    ) %>% 
    filter(
        season == 2021, 
        !round %in% c('EF','GF','PF','QF','SF')
    ) %>% distinct()

form_custom <- afltables_custom %>% 
    group_by(playing_for, w_l) %>% 
    summarise(
        s_score = sum(playing_for_score),
        s_opp_score = sum(opp_score),
        games = n(),
        .groups = 'drop'
    )

custom_ladder <- form_custom %>% 
    pivot_wider(
        names_from = w_l,
        values_from = games,
        values_fill = list(games = 0)
    ) %>%
    group_by(playing_for) %>% 
    summarise(
        W = sum(W),
        D = sum(D),
        L = sum(L),
        P = sum(s_score - s_opp_score),
        .groups = 'drop'
    ) %>% arrange(-P) # + D*2

write.csv(custom_ladder, 'aflxepl_2020.csv')
View(custom_ladder)


afltables %>% 
    select(season, round, playing_for, playing_for_score, w_l) %>% distinct() %>% 
    filter(playing_for_score >= 100, w_l == 'L') %>% 
    group_by(season, playing_for) %>% 
    summarise(
        n = n()
    ) %>% View()

afltables %>% 
    select(season, date, id, first_name, surname, opp) %>% 
    group_by(id, first_name, surname) %>% 
    mutate(m_season = max(season)) %>% 
    filter(m_season == 2021) %>% 
    summarise(
        games_played = n(),
        n_unique_teams = length(unique(opp)),
        teams = paste(unique(opp), collapse = ', ')
    ) %>% View()

afltables %>% 
    select(season, date, round, id, first_name, surname, games_played, venue) %>%
    filter(venue == 'M.C.G.') %>% 
    group_by(id, first_name, surname) %>% 
    filter(date == min(date)) %>% 
    View()

afltables %>% 
    select(season, round, date, playing_for, playing_for, id, first_name, surname) %>% 
    filter(substring(first_name,1,1) == substring(surname,1,1)) %>%
    group_by(season, round, date, playing_for) %>% 
    summarise(
        n = n(),
        names = paste(first_name, surname, collapse = ', ')
    ) %>% View()
    

