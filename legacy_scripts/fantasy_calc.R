# devtools::install_github("jimmyday12/fitzRoy")
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- get_afltables_stats()

afltables %>% filter(Surname == 'Rockliff') %>%
  mutate(
    fantasy_points = Kicks*3 + 
      Handballs*2 +
      Marks*3 +
      Tackles*4 +
      Frees.For + 
      Frees.Against*-3 +
      Hit.Outs +
      Goals*6 +
      Behinds
  ) %>% 
  select(fantasy_points) %>% 
  arrange(-fantasy_points) %>% View()


