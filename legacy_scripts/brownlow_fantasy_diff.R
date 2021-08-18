# devtools::install_github("jimmyday12/fitzRoy")
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- get_afltables_stats()
names(afltables) <- names(afltables) %>% snakecase::to_snake_case()

afltables_modern <- afltables %>% 
  filter(date > "1984-01-01") 

afltables_modern_fant <- afltables_modern %>% 
  mutate(
    fantasy_points = kicks*3 + 
      handballs*2 +
      marks*3 +
      tackles*4 +
      frees_for + 
      frees_against*-3 +
      hit_outs +
      goals*6 +
      behinds
  )

afltables_modern_fant %>% select(season, round, date, first_name, surname, playing_for, fantasy_points, brownlow_votes) %>% 
  filter(brownlow_votes == 0) %>% arrange(-fantasy_points) %>% View()
  
afltables_modern_fant %>% select(season, round, date, first_name, surname, playing_for, fantasy_points, brownlow_votes) %>% 
  filter(brownlow_votes %in% c(1,2,3)) %>% arrange(fantasy_points) %>% View()
  