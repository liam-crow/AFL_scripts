
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% select(date, bounces) %>% filter(bounces > 0) %>% arrange(date)

afltables %>% filter(date > "1991-01-01") %>% 
    select(id, first_name, surname, season, round, kicks, handballs, bounces) %>% 
    mutate(
        disp = kicks + handballs, 
        diff = kicks - bounces
    ) %>% 
    arrange(diff) %>% View()
