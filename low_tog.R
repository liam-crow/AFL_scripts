
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- fitzRoy::get_afltables_stats(start_date = "1900-01-01")
names(afltables) <- snakecase::to_snake_case(names(afltables))

afltables %>% filter(date > '2003-01-01') %>% 
    select(date, season, round, id, first_name, surname, kicks, handballs, time_on_ground) %>% 
    mutate(disp = kicks + handballs) %>% 
    filter(disp == 1) %>% arrange(time_on_ground)

afltables %>% filter(date > '2003-01-01') %>% 
    group_by(date, season, round, playing_for) %>% 
    summarise(tog = sum(time_on_ground)) %>% arrange(-tog)


