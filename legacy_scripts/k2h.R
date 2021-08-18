# devtools::install_github("jimmyday12/fitzRoy")
library(fitzRoy)
library(dplyr)
library(lubridate)
library(zoo)

afl_2019 <- fitzRoy::get_afltables_stats(start_date = "2019-01-01", end_date = "2020-01-01")

afl_2019 %>% select(ID, First.name, Surname, Kicks, Marks, Handballs) %>% 
  group_by(ID, First.name, Surname) %>% 
  summarise(
    kicks_sum = sum(Kicks),
    marks_sum = sum(Marks),
    hball_sum = sum(Handballs),
    game_count = n()
  ) %>% 
  filter(game_count > 3) %>% 
  mutate(
    k2m = kicks_sum/marks_sum,
    k2h = kicks_sum/hball_sum,
    m2h = marks_sum/hball_sum
  ) %>% 
  arrange(-k2m)
