library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
source("load_afltables.R")
devtools::install_github("JaseZiv/worldfootballR")
library(worldfootballR)

epl <- get_match_results(country = "ENG", gender = "M", season_end_year = 2010:2021, tier = "1st")
names(epl) <- snakecase::to_snake_case(names(epl))


epl_team <- 'Leicester City'
epl_leicester <- epl %>% 
    select(wk, date, home, away, home_goals, away, away_goals, venue) %>% 
    filter(home == epl_team | away == epl_team) %>% 
    mutate(
        epl_w_l = case_when(
            home == epl_team & home_goals > away_goals ~ 'W',
            home == epl_team & home_goals < away_goals ~ 'L',
            away == epl_team & home_goals < away_goals ~ 'W',
            away == epl_team & home_goals > away_goals ~ 'L',
            T ~ 'D'
        )
    )

afltables_lester <- afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname,venue, goals, h_a, playing_for, opp, playing_for_score, opp_score, w_l) %>% 
    filter(surname == 'Lester', season > 2000)

inner_join(afltables_lester, epl_leicester, by = 'date') %>% View()
