library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
source("load_afltables.R")
source("load_fryzigg.R")

fryzigg_data %>% 
    select(season, date, round, id, first_name, surname, fantasy_points, afl_fantasy_score) %>% 
    filter(fantasy_points != afl_fantasy_score)
