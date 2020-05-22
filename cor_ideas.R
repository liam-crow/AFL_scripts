
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)
library(fitzRoy)

source("load_afltables.R")

id_time_disp <- afltables %>% 
    filter(id == 11548) %>% 
    select(id, kicks, handballs)

plot(id_time_disp$kicks, id_time_disp$handballs)

afltables %>% 
    filter(season > 1965) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        cor = cor(kicks, handballs),
        games = n(),
        t = sum(disposals)
    ) %>% 
    filter(games > 200) %>% 
    mutate(
        diff = abs(cor)
    ) %>% 
    arrange(diff)

