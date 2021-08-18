# devtools::install_github("jimmyday12/fitzRoy")
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- get_afltables_stats()

afltables %>% select(ID, First.name, Surname, Playing.for, Home.team, Away.team, Home.score, Away.score) %>% 
  mutate(
    WL = case_when(
      Playing.for == Home.team & Home.score > Away.score ~ "W",
      Playing.for == Home.team & Home.score < Away.score ~ "L",
      Playing.for == Away.team & Home.score < Away.score ~ "W",
      Playing.for == Away.team & Home.score > Away.score ~ "L",
      TRUE ~ 'D'
    )
  ) %>% 
  group_by(ID, First.name, Surname, WL) %>% 
  count() %>% 
  pivot_wider(
    id_cols = c('ID', 'First.name', 'Surname'),
    names_from = WL,
    values_from = n,
    values_fill = list(n = 0)
  ) %>% 
  mutate(
    t_games = L+W+D,
    D_ratio = D/(L+W+D)
  ) %>% 
  arrange(-D_ratio) %>% View()




