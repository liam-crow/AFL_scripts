library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% select(date, id, first_name, surname, playing_for, kicks, handballs) %>% 
    mutate(disp = kicks + handballs) %>% 
    group_by(id, first_name, surname, playing_for) %>% 
    arrange(date) %>% 
    summarise(
        # playing_for = paste(unique(playing_for), collapse = ', '),
        cons_handballs = paste(handballs, collapse = ', '),
        cons_kicks = paste(kicks, collapse = ', '),
        cons_disp = paste(disp, collapse = ', ')
    ) %>% View()

# 4, 18, 8, 20, 21
# 21, 20, 8, 18, 4

# 18, 8, 20, 21, 9
# 9, 21, 20, 8, 18