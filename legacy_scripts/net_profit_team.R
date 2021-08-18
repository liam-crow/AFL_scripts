# devtools::install_github("jimmyday12/fitzRoy")

library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)

afltables <- fitzRoy::get_afltables_stats()

names(afltables) <- to_snake_case(names(afltables))

betting_odds_2019 <- get_footywire_betting_odds(start_season = "2010") %>% as_tibble()

names(betting_odds_2019) <- snakecase::to_snake_case(names(betting_odds_2019))

home_teams <- betting_odds_2019 %>% select(date, round, venue, team = home_team, team_paid = home_win_paid)
away_teams <- betting_odds_2019 %>% select(date, round, venue, team = away_team, team_paid = away_win_paid)

all_teams_paid <- rbind(home_teams, away_teams)

net_per_team <- all_teams_paid %>% 
    mutate(
        winnings = case_when(
            team_paid == 0 ~ -10,
            team_paid > 0  ~ (team_paid*10)-10
        )
    ) %>% 
    group_by(team) %>% 
    summarise(net = sum(winnings)) %>% 
    arrange(-net)

net_per_team %>% summarise(sum(net))
