library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)

betting_odds_raw <- fetch_betting_odds_footywire(start_season = "2010") %>% as_tibble()

names(betting_odds_raw) <- snakecase::to_snake_case(names(betting_odds_raw))

betting_odds <- betting_odds_raw %>% 
    mutate(
        season = lubridate::year(date),
        team_1 = if_else(home_team > away_team, home_team, away_team),
        team_2 = if_else(home_team < away_team, home_team, away_team),
        team_1_odds = if_else(home_team > away_team, home_win_odds, away_win_odds),
        team_2_odds = if_else(home_team < away_team, home_win_odds, away_win_odds),
        team_1_win_pcnt = 0.95/team_1_odds*100,
        team_2_win_pcnt = 0.95/team_2_odds*100,
    )
    

# betting_odds %>% 
#     select(season, date, round, team_1, team_2, team_1_win_pcnt, team_2_win_pcnt) %>% 
#     group_by(season, team_1, team_2) %>% 
#     arrange(date) %>% 
#     mutate(
#         n = n(),
#         team_1_win_pcnt_lag = lag(team_1_win_pcnt),
#         team_2_win_pcnt_lag = lag(team_2_win_pcnt),
#         round_lag = lag(round),
#         abs_win_p_diff = abs(team_1_win_pcnt - lag(team_1_win_pcnt))
#     ) %>% View()

# 0.95/2.47*100-0.95/1.14*100 richmond r2 to r23

# 38.4% r23
# 83.3% r2
