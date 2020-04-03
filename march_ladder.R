# devtools::install_github("jimmyday12/fitzRoy")
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- get_afltables_stats()
names(afltables) <- names(afltables) %>% snakecase::to_snake_case()

afltables_march <- afltables %>% 
    select(season, round, date, home_team, away_team, home_score, away_score) %>% 
    filter(month(date) == 3) %>% 
    distinct() 

march_home <- afltables_march %>% 
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

march_away <- afltables_march %>% 
    select(team = away_team, home_score, away_score)%>% 
    mutate(
        WL = case_when(
            home_score < away_score ~ 'W',
            home_score > away_score ~ 'L',
            TRUE ~ 'D'
        ),
        score = away_score,
        opp_score = home_score
    )

comb_march <- rbind(march_home, march_away)

form_march <- comb_march %>% 
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

custom_ladder <- form_march %>% 
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
        `%` = sum(score)/sum(opp_score)*100
    ) %>% arrange(-`%`)

write.csv(custom_ladder, 'march_ladder.csv')
