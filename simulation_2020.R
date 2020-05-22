
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)
library(fitzRoy)

source("load_afltables.R")

fixture_2020 <- readr::read_csv("https://fixturedownload.com/download/afl-2020-AUSEasternStandardTime.csv")
names(fixture_2020) <- c('round', 'date', 'location', 'home_team', 'away_team', 'result')

fixture_2020_new <- fixture_2020 %>% 
    select(-result) %>% 
    mutate(
        home_team = mgsub::mgsub(
            home_team,
            c('Gold Coast Suns', 'GWS Giants', 'Geelong Cats', 'Adelaide Crows', 'Sydney Swans', 'West Coast Eagles'),
            c('Gold Coast', 'Greater Western Sydney', 'Geelong', 'Adelaide', 'Sydney', 'West Coast')
        ),
        away_team = mgsub::mgsub(
            away_team,
            c('Gold Coast Suns', 'GWS Giants', 'Geelong Cats', 'Adelaide Crows', 'Sydney Swans', 'West Coast Eagles'),
            c('Gold Coast', 'Greater Western Sydney', 'Geelong', 'Adelaide', 'Sydney', 'West Coast')
        )
    )

# separate(result, c('home_score', 'away_score'), sep = ' - ')
unique(afltables$home_team)
unique(fixture_2020_new$home_team)


hist_sim <- afltables %>%
    filter(between(season, 2000, 2019)) %>% 
    select(date, season, round, home_team, away_team, home_score, away_score) %>% distinct() %>% 
    filter(!(round %in% c("GF","SF","PF","EF",'QF'))) %>% 
    group_by(home_team, away_team) %>% 
    arrange(desc(date)) %>% 
    filter(row_number() %in% 1:4) %>% #count() %>% View()
    summarise(
        home_avg = mean(home_score),
        away_avg = mean(away_score)
    )

inner_join(fixture_2020_new, hist_sim, by = c('home_team', 'away_team')) %>% 
    rowwise() %>% 
    mutate(
        home_score = rpois(1, home_avg),
        away_score = rpois(1, away_avg)
    ) %>% 
    mutate(
        w_l = case_when(
            team_1 > team_2 ~ "W",
            team_1 < team_2 ~ "L",
            TRUE ~ "D"
        )
    )



tibble(
    team_1 = rpois(1000, 90),
    team_2 = rpois(1000, 70)
) %>% mutate(
    w_l = case_when(
        team_1 > team_2 ~ "W",
        team_1 < team_2 ~ "L",
        TRUE ~ "D"
    )
) %>% 
    group_by(w_l) %>% 
    count()

library(ggplot2)

ggplot(as.data.frame(dat), aes(x = x)) + geom_density()


