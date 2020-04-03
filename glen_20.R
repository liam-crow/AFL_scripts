
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- fitzRoy::get_afltables_stats(start_date = "1900-01-01")
names(afltables) <- snakecase::to_snake_case(names(afltables))

player_id <- afltables %>% 
    select(id, first_name, surname) %>% 
    distinct()

afltables %>% select(season, round, id, first_name, surname, jumper_no, playing_for, kicks, handballs) %>% 
    filter(
        # first_name == 'Glen' | first_name == 'Glenn' | surname == stringr::str_match(surname, '[Glen]')
        grepl('Glen', first_name) | grepl('Glen', surname)
    ) %>% mutate(disp = kicks + handballs) %>% filter(disp == 20) %>% 
    group_by(id, first_name, surname, playing_for, jumper_no) %>% count() %>% arrange(-n) %>% View()
    
