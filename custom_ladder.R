
library(dplyr)
library(tidyr)
library(lubridate)
source("load_afltables.R")

#### Generic Ladder ####

afltables_custom <- afltables %>% 
    select(season, round, date, playing_for, playing_for_score, opp_score, w_l) %>% 
    filter(
        date > "2021-01-01", 
        # !round %in% c('EF','GF','PF','QF','SF')
    ) %>% 
    distinct() #%>% 
# mutate(
#     playing_for_score = pq_4_
# )

form_custom <- afltables_custom %>% 
    mutate(
        playing_for = case_when(
            playing_for == 'Brisbane Bears' ~ 'Brisbane Lions',
            playing_for == 'Fitzroy' ~ 'Brisbane Lions',
            TRUE ~ playing_for
        )
    ) %>% 
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
        # D = sum(D),
        L = sum(L),
        `%` = round(sum(s_score)/sum(s_opp_score)*100, 2),
        .groups = 'drop'
    ) %>% 
    mutate(P = W*4) %>% arrange(-P, -`%`) # + D*2

# write.csv(custom_ladder, 'march_ladder.csv')
View(custom_ladder)

#### EPL Ladder ####

winners <- NULL
for (year in 1990:2019) {
    
    afltables_custom <- 
        afltables %>% 
        select(season, round, date, home_team, away_team, home_score = hq_4_g, away_score = aq_4_g) %>% 
        filter(
            season == year, 
            !round %in% c('EF','GF','PF','QF','SF')
        ) %>% 
        distinct() 
    
    cus_home <- afltables_custom %>% 
        select(team = home_team, home_score, away_score) %>% 
        mutate(
            WL = case_when(
                home_score > away_score ~ 'W',
                home_score < away_score ~ 'L',
                TRUE ~ 'D'
            ),
            score = home_score,
            opp_score = away_score
        )
    
    cus_away <- afltables_custom %>% 
        select(team = away_team, home_score, away_score) %>% 
        mutate(
            WL = case_when(
                home_score < away_score ~ 'W',
                home_score > away_score ~ 'L',
                TRUE ~ 'D'
            ),
            score = away_score,
            opp_score = home_score
        )
    
    comb_custom <- rbind(cus_home, cus_away)
    
    form_custom <- comb_custom %>% 
        group_by(team, WL) %>% 
        summarise(
            score = sum(score),
            opp_score = sum(opp_score),
            games = n(),
            .groups = 'drop'
        )
    
    custom_ladder <- 
        form_custom %>% 
        pivot_wider(
            names_from = WL,
            values_from = games,
            values_fill = list(games = 0)
        ) %>% 
        group_by(team) %>% 
        summarise(
            W = sum(W),
            D = sum(D),
            L = sum(L),
            GD = sum(score) - sum(opp_score),
            .groups = 'drop'
        ) %>% 
        mutate(P = W*3 + D*1) %>% arrange(-P, -GD) # + D*2
    
    winner <- custom_ladder %>% filter(row_number() == 1) %>% mutate(season = year)
    
    winners <- rbind(winners, winner)
}

winners %>% select(season, team) %>% group_by(team) %>% count() %>% 
    arrange(-n)

# write.csv(custom_ladder, 'AFL_EPL.csv')


#### Goals/Behinds reversed ####

afltables_custom <- afltables %>% 
    select(season, round, date, playing_for, pq_4_g, pq_4_b, oq_4_g, oq_4_b) %>% 
    filter(
        date > "2020-01-01", 
        # !round %in% c('EF','GF','PF','QF','SF')
    ) %>% 
    distinct() %>% 
    mutate(
        playing_for_score = pq_4_g + pq_4_b*6,
        opp_score = oq_4_g + oq_4_b*6,
        w_l = case_when(
            playing_for_score > opp_score ~ 'W',
            playing_for_score < opp_score ~ 'L',
            TRUE ~ 'D'
        )
    )

form_custom <- afltables_custom %>% 
    mutate(
        playing_for = case_when(
            playing_for == 'Brisbane Bears' ~ 'Brisbane Lions',
            playing_for == 'Fitzroy' ~ 'Brisbane Lions',
            TRUE ~ playing_for
        )
    ) %>% 
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
        `%` = round(sum(s_score)/sum(s_opp_score)*100, 2),
        .groups = 'drop'
    ) %>% 
    mutate(P = W*4 + D*2) %>% arrange(-P, -`%`) # + D*2
write.csv(custom_ladder, "reversed_goals_behinds_ladder.csv")

#### AFL Boost Point ####

afltables_custom <- afltables %>% 
    select(season, round, date, playing_for, pq_2_g, pq_2_b, oq_2_g, oq_2_b, pq_4_g, pq_4_b, oq_4_g, oq_4_b) %>% 
    filter(
        date > "2020-01-01", 
        !round %in% c('EF','GF','PF','QF','SF')
    ) %>% 
    distinct() %>% 
    mutate(
        playing_for_ht_score = pq_2_g*6 + pq_2_b,
        opp_ht_score = oq_2_g*6 + oq_2_b,
        playing_for_score = pq_4_g*6 + pq_4_b,
        opp_score = oq_4_g*6 + oq_4_b,
        w_l = case_when(
            playing_for_score > opp_score ~ 'W',
            playing_for_score < opp_score ~ 'L',
            TRUE ~ 'D'
        ),
        ht_w_l = case_when(
            playing_for_ht_score > opp_ht_score ~ 'ht_W',
            playing_for_ht_score < opp_ht_score ~ 'ht_L',
            TRUE ~ 'ht_D'
        )
    )

form_custom <- afltables_custom %>% 
    mutate(
        playing_for = case_when(
            playing_for == 'Brisbane Bears' ~ 'Brisbane Lions',
            playing_for == 'Fitzroy' ~ 'Brisbane Lions',
            TRUE ~ playing_for
        )
    ) %>% 
    group_by(playing_for, w_l, ht_w_l) %>% 
    summarise(
        s_score = sum(playing_for_score),
        s_opp_score = sum(opp_score),
        games = n(),
        .groups = 'drop'
    )

custom_ladder <- form_custom %>% 
    pivot_wider(
        names_from = c('w_l', 'ht_w_l'),
        values_from = games,
        values_fill = list(games = 0)
    ) %>% 
    group_by(playing_for) %>% 
    summarise(
        L_ht_L = sum(L_ht_L),
        L_ht_W = sum(L_ht_W),
        W_ht_W = sum(W_ht_W),
        W_ht_L = sum(W_ht_L),
        D_ht_W = sum(D_ht_W),
        L_ht_D = sum(L_ht_D),
        D_ht_L = sum(D_ht_L),
        W_ht_D = sum(W_ht_D), 
        # D_ht_D = sum(D_ht_D), #not sure if all possible combinations
        `%` = round(sum(s_score)/sum(s_opp_score)*100, 2),
        .groups = 'drop'
    ) %>% 
    mutate(P = L_ht_W*1 + W_ht_W*4 + W_ht_L*3 + D_ht_W*3 + D_ht_L*1 + W_ht_D*4) %>% arrange(-P, -`%`) # + D_ht_D*2
write.csv(custom_ladder, "afl_boost_ladder.csv")

#Wet weather ladder
source("fryzigg_data.R")
afltables_custom <- fryzigg_data %>% 
    mutate(
        playing_for_score = if_else(player_team == match_home_team, match_home_team_score, match_away_team_score),
        opp_score = if_else(player_team == match_home_team, match_away_team_score, match_home_team_score),
        w_l = case_when(
            playing_for_score > opp_score ~ "W",
            playing_for_score < opp_score ~ "L",
            TRUE ~ "D"
        )
    ) %>% 
    select(season, date, match_round, match_date, match_weather_type, playing_for = player_team, playing_for_score, opp_score, w_l) %>% 
    distinct() %>%
    filter(
        date > "2010-01-01", 
        match_weather_type %in% c("WINDY_RAIN", "RAIN", "THUNDERSTORMS")
        # !round %in% c('EF','GF','PF','QF','SF')
    )

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
        `%` = round(sum(s_score)/sum(s_opp_score)*100, 2),
        .groups = 'drop'
    ) %>% 
    mutate(win_rate = round(W/(W + D + L)*100, 2)) %>% arrange(-win_rate, -`%`) # + D*2

View(custom_ladder)
write.csv(custom_ladder, "afl_all_weather_ladder.csv")

#### EPL ladder ####

afltables_custom <- afltables %>% 
    select(season, round, date, playing_for, pq_4_g, oq_4_g) %>% 
    mutate(
        w_l = case_when(
            pq_4_g > oq_4_g ~ 'W',
            pq_4_g < oq_4_g ~ 'L',
            T ~ 'D',
        )
    ) %>% 
    filter(
        season == 2020, 
        !round %in% c('EF','GF','PF','QF','SF')
    ) %>% 
    distinct()


form_custom <- afltables_custom %>% 
    group_by(playing_for, w_l) %>% 
    summarise(
        s_score = sum(pq_4_g),
        s_opp_score = sum(oq_4_g),
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
        GD = sum(s_score - s_opp_score),
        .groups = 'drop'
    ) %>% 
    mutate(P = W*3 + D*1) %>% arrange(-P, -GD) # + D*2

write.csv(custom_ladder, 'aflxepl_2020.csv')
View(custom_ladder)

#### multiplicative ladder ####

afltables_custom <- afltables %>% 
    select(season, round, date, playing_for, pq_4_g, pq_4_b, oq_4_g, oq_4_b) %>% 
    filter(
        season == 2021,
        !round %in% c('EF','GF','PF','QF','SF')
    ) %>% 
    distinct() %>% 
    mutate(
        playing_for_score = pq_4_g / pq_4_b,
        opp_score = oq_4_g / oq_4_b,
        w_l = case_when(
            playing_for_score > opp_score ~ 'W',
            playing_for_score < opp_score ~ 'L',
            TRUE ~ 'D'
        )
    )

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
        `%` = round(sum(s_score)/sum(s_opp_score)*100, 2),
        .groups = 'drop'
    ) %>% 
    mutate(P = W*4 + D*2) %>% arrange(-P, -`%`) # + D*2
write.csv(custom_ladder, "custom_ladders/division_ladder.csv")

#### finals ladder since Ess won ####

afltables_custom <- afltables %>% 
    select(date, season, round, date, playing_for, playing_for_score, opp_score, w_l) %>% 
    filter(
        date > "2004-09-04",
        round %in% c('EF','GF','PF','QF','SF')
    ) %>% 
    distinct()

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
        `%` = round(sum(s_score)/sum(s_opp_score)*100, 2),
        .groups = 'drop'
    ) %>% 
    mutate(P = W*4 + D*2) %>% arrange(-P, -`%`) # + D*2

write.csv(custom_ladder, "custom_ladders/after_ess_fin_win.csv", row.names = F)

#### 2022 ladder 3qt ####

afltables_custom <- afltables %>% 
    select(date, season, round, date, playing_for, pq_3_g, pq_3_b, oq_3_g, oq_3_b, opp_score) %>% 
    filter(season == 2022) %>% 
    distinct() %>% 
    mutate(
        playing_for_score = pq_3_g*6 + pq_3_b,
        opp_score = oq_3_g*6 + oq_3_b,
        w_l = case_when(
            playing_for_score > opp_score ~ 'W',
            playing_for_score < opp_score ~ 'L',
            TRUE ~ 'D'
        )
    )

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
        `%` = round(sum(s_score)/sum(s_opp_score)*100, 2),
        .groups = 'drop'
    ) %>% 
    mutate(P = W*4+ D*2) %>% arrange(-P, -`%`) # + D*2

write.csv(custom_ladder, "custom_ladders/QT3_score.csv", row.names = F)

