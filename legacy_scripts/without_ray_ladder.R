# devtools::install_github("jimmyday12/fitzRoy")
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- get_afltables_stats()
names(afltables) <- names(afltables) %>% snakecase::to_snake_case()

afltables %>% select(umpire_1, umpire_2, umpire_3, umpire_4) %>% View()

afltables_custom <- afltables %>% 
    filter(
        date > '2019-01-01', date < '2020-01-01',
        !round %in% c('EF','GF','PF','QF','SF')
        # (umpire_1 == "Ray Chamberlain" | umpire_2 == "Ray Chamberlain" |
            # umpire_3 == "Ray Chamberlain" | umpire_4 == "Ray Chamberlain")
    ) %>% 
    select(season, round, date, home_team, away_team, home_score, away_score, starts_with('umpire')) %>% 
    unite(umpires, umpire_1, umpire_2, umpire_3, umpire_4, sep = ',') %>% 
    filter(!grepl('Ray', umpires)) %>% 
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

write.csv(custom_ladder, 'without_ray_ladder.csv')
