# devtools::install_github("jimmyday12/fitzRoy")

library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)

afltables <- fitzRoy::get_afltables_stats()

names(afltables) <- to_snake_case(names(afltables))

afl_id <- afltables_all %>% select(id, first_name, surname) %>% distinct()


#### games by games played with time on ground ####
res <- afltables %>% select(season, round, date, time_on_ground, id, first_name, surname, playing_for, time_on_ground) %>% 
    group_by(id, first_name, surname) %>% arrange(date) %>% 
    mutate(games_running_count = row_number()) %>% 
    filter(
        time_on_ground == games_running_count,
        time_on_ground %in% c(100)
    ) %>% ungroup() %>% mutate(comb = paste(first_name, surname)) #%>% 
paste(res$comb, sep = " ", collapse = ", ")


#### games with disp = games ####
afltables %>% select(season, round, date, id, first_name, surname, playing_for, kicks, handballs) %>%
    mutate(disp = kicks + handballs) %>% 
    group_by(disp) %>% arrange(date) %>% 
    mutate(running_count = row_number()) %>% 
    filter(
        # disp == running_count
    ) %>% 
    group_by(disp) %>% count() %>% View()

#### game milestones in same game ####
afltables %>% select(season, round, date, id, first_name, surname, playing_for) %>%
    group_by(id, first_name, surname) %>% arrange(date) %>% 
    mutate(running_count = row_number()) %>% 
    filter(running_count %in% c(50,100,150,200,250,300,350,400)) %>% 
    group_by(date, playing_for) %>% 
    mutate(count = n()) %>% 
    filter(between(count, 2, 19), season > 2009) %>% View()


afltables %>% select(season, round, date, time_on_ground, id, first_name, surname, playing_for, time_on_ground) %>% 
    filter(time_on_ground == 50) %>% 
    group_by(id, first_name, surname) %>% count() %>% arrange(-n)
