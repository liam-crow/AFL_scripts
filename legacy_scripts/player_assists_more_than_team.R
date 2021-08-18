# devtools::install_github("jimmyday12/fitzRoy")

library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)

afltables <- fitzRoy::get_afltables_stats()

names(afltables) <- to_snake_case(names(afltables))

afltables_team_assists <- 
    afltables %>% filter(date > "2003-01-01") %>% 
    select(season, round, date, home_team, away_team, first_name, surname, playing_for, goal_assists) %>% 
    group_by(season, round, date, home_team, away_team, playing_for) %>% 
    summarise(total_ga = sum(goal_assists), player_ga = max(goal_assists))

afltables_team_assists %>% 
    summarise(diff1 = total_ga[1] - player_ga[2], diff2 = total_ga[2] - player_ga[1]) %>% 
    filter(diff1 < 0 | diff2 < 0) %>% View()

afltables_team_assists %>% 
    summarise(diff1 = total_ga[1] - player_ga[2], diff2 = total_ga[2] - player_ga[1]) %>% 
    filter(diff1 == -3 | diff2 == -3) %>% View()


# 2011	19	2011-07-30	Geelong	Melbourne	29	-5
afltables %>% 
    select(season, round, date, venue, home_team, away_team, first_name, surname, playing_for, goal_assists, goals, home_score, away_score) %>% 
    filter(season == 2011, round == '19', date == '2011-07-30', home_team == "Geelong", away_team == "Melbourne") %>% 
    group_by(playing_for) #%>% summarise(sum(goal_assists))


# 2016	EF	2016-09-08	Western Bulldogs	West Coast
afltables %>% 
    select(season, round, date, venue, home_team, away_team, first_name, surname, playing_for, goal_assists, goals, home_score, away_score) %>% 
    filter(season == 2016, round == 'EF', date == '2016-09-08', home_team == "Western Bulldogs", away_team == "West Coast") %>% View()
    group_by(playing_for) #%>% summarise(sum(goal_assists))
