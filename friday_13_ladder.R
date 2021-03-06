# devtools::install_github("jimmyday12/fitzRoy")
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- get_afltables_stats()
names(afltables) <- names(afltables) %>% snakecase::to_snake_case()

weekdays(afltables$date)

afltables_custom <- afltables %>% 
    select(season, round, date, home_team, away_team, home_score, away_score) %>% 
    filter(day(date) == 13 & weekdays(date) == 'Friday') %>% 
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
    mutate(
        team = case_when(
            team == 'Brisbane Bears' ~ 'Brisbane Lions',
            team == 'Fitzroy' ~ 'Brisbane Lions',
            TRUE ~ team
        )
    ) %>% 
    group_by(team, WL) %>% 
    summarise(
        score = sum(score),
        opp_score = sum(opp_score),
        games = n()
    )

custom_ladder <- form_custom %>% 
    pivot_wider(
        names_from = WL,
        values_from = games,
        values_fill = list(games = 0)
    ) %>% 
    group_by(team) %>% 
    summarise(
        W = sum(W),
        # D = sum(D),
        L = sum(L),
        `%` = sum(score)/sum(opp_score)*100
    ) %>% 
    mutate(P = W*4) %>% arrange(-P, -`%`) # + D*2

write.csv(custom_ladder, 'friday_13_ladder.csv')
