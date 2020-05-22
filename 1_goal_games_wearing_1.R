library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)
library(fitzRoy)

source("load_afltables.R")

afltables %>% 
    select(id, first_name, surname, playing_for, jumper_no, goals) %>% 
    filter(jumper_no == 1, goals == 1) %>% 
    group_by(id, first_name, surname, jumper_no) %>% 
    count() %>% 
    arrange(-n)
