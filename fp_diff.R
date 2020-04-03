# devtools::install_github("jimmyday12/fitzRoy")

library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)

afltables <- fitzRoy::get_afltables_stats(start_date = "1950-01-01")

names(afltables) <- to_snake_case(names(afltables))

afl_player_id <- afltables_all %>% group_by(id, first_name, surname) %>% 
  summarise(start_date = min(date)) %>% filter(start_date > "1990-01-01") %>% 
  ungroup()

afltables_mod <- afltables %>% inner_join(afl_player_id, by = c("id", "first_name", "surname"))

fantasy_agg <- afltables_mod %>% 
  mutate(
    fantasy = kicks*3 +
      handballs*2 +
      marks*3 +
      tackles*4 +
      frees_for*1 +
      frees_against*-3 + 
      hit_outs*1 +
      goals*6 +
      behinds*1
  ) %>% 
  group_by(id, first_name, surname, playing_for) %>% 
  summarise(
    avg_fantasy = mean(fantasy),
    n = n(),
    sdate = min(date),
    edate = max(date)
  )

fantasy_team_agg <- fantasy_agg %>% 
  group_by(id, first_name, surname) %>% 
  summarise(
    fantasy_diff = max(avg_fantasy) - min(avg_fantasy),
    teams = n(),
    team_names = paste(sdate,'to', edate, playing_for, round(avg_fantasy,1), n,'g', collapse = ', ')
  ) %>% filter(teams != 1)

fantasy_team_agg %>% arrange(-fantasy_diff)

