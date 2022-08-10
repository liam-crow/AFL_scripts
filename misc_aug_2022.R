
library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
library(stringr)
library(ggplot2)
source("load_afltables.R")
source("load_fryzigg.R")

afltables %>% 
    select(season, round, playing_for, id, first_name, surname, games_played) %>% 
    filter(games_played >= 250) %>% 
    group_by(season, round, playing_for) %>% 
    summarise(
        n = n(),
        comb = paste0(surname, ' (',games_played,')', collapse = ', ')
    ) %>% View()


