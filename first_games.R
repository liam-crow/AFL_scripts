# devtools::install_github("jimmyday12/fitzRoy")
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- get_afltables_stats()
names(afltables) <- names(afltables) %>% snakecase::to_snake_case()


afl_round_starts <- afltables %>% 
  select(date, season, round, id, first_name, surname) %>% 
  # distinct() %>% 
  group_by(id, first_name, surname) %>% 
  filter(date == min(date)) %>% ungroup() %>% 
  group_by(round) %>% 
  count() %>% arrange(-n)
  
View(afl_round_starts)

afl_round_starts %>% 
  ungroup() %>% summarise(sum = sum(n))

afltables %>% 
  # select(date, season, round, id, first_name, surname, playing_for, home_team, away_team, home_score, away_score) %>% 
  # # distinct() %>% 
  # group_by(id, first_name, surname) %>% 
  # filter(date %in% order(date, decreasing = T)[1:2]) #%>% 
  # mutate(diff = home_score - away_score) %>% 
  # filter(diff == 0) %>% 
  View()

order()
