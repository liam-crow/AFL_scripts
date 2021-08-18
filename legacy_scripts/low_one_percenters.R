
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% 
    filter(date > '1999-01-01') %>% 
    select(date, id, first_name, surname, one_percenters, kicks, handballs) %>% 
    filter(
        # grepl('^op', first_name, ignore.case = T),
        # grepl('op', surname, ignore.case = T),
        T
    ) %>% group_by(id, first_name, surname) %>% 
    summarise(
        debut = min(date),
        ret = max(date),
        count = n(),
        total_op = sum(one_percenters),
        total_d = sum(kicks) + sum(handballs)
    ) %>% 
    mutate(ratio = total_op/count) %>% 
    arrange(-ratio)
