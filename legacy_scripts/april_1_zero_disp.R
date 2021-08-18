
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- fitzRoy::get_afltables_stats(start_date = "1900-01-01")
names(afltables) <- snakecase::to_snake_case(names(afltables))

afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname, kicks, handballs, marks, home_team, home_score, away_team, away_score) %>% 
    filter(month(date) == 4, day(date) == 1) %>% 
    mutate(
        disp = kicks + handballs
    ) %>% 
    filter(disp == 0)
