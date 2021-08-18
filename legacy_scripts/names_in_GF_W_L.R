
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)
library(fitzRoy)

source("load_afltables.R")

afltables %>% 
    filter(round == 'GF', w_l == 'L') %>% 
    group_by(season, playing_for, first_name) %>% 
    count() %>% View()
