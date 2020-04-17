
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% 
    select(date, id, first_name, surname, kicks, handballs, marks, tackles) %>% 
    group_by(id, first_name, surname) %>% arrange(date) %>% 
    mutate(
        debut = min(date),
        game_no = row_number()
    ) %>% ungroup() %>% 
    filter(debut > '1965-01-01') %>% 
    mutate(disp = kicks + handballs) %>% 
    filter(tackles == game_no) %>% 
    group_by(id, first_name, surname) %>% count() %>% View()

diff_between_disp <- afltables %>% 
    select(date, id, first_name, surname, kicks, handballs, marks, tackles) %>% 
    group_by(id, first_name, surname) %>% arrange(date) %>% 
    mutate(
        disp = kicks + handballs,
        debut = min(date),
        game_no = row_number()
    ) %>% ungroup() %>% 
    filter(debut > '1965-01-01') %>% 
    group_by(id, first_name, surname, disp) %>% 
    mutate(
        game_diff = game_no - lag(game_no, 1)
    )

write.csv(diff_between_disp, 'diff_between_disp.csv')
