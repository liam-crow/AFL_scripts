
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)
library(fitzRoy)

source("load_afltables.R")

afltables %>% 
    filter(season > 1986) %>% 
    select(id, first_name, surname, jumper_no) %>% 
    distinct() %>% #View()
    group_by(id, first_name, surname) %>% 
    summarise(
        sum_j_no = sum(jumper_no)
    ) %>% 
    arrange(-sum_j_no)
    

