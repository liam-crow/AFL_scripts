
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- fitzRoy::get_afltables_stats(start_date = "1900-01-01")
names(afltables) <- snakecase::to_snake_case(names(afltables))

#kicks
afltables %>% group_by(id, first_name, surname) %>% 
    summarise(
        disp = sum(kicks) + sum(handballs),
        debut = min(date),
        # mrg = max(date)
    ) %>% arrange(-disp) %>% 
    filter(
        debut > '1965-01-01',
        disp >= 1410
    ) %>% nrow()

#marks
afltables %>% group_by(id, first_name, surname) %>% 
    summarise(
        marks = sum(marks),
        debut = min(date),
        # mrg = max(date)
    ) %>% arrange(-marks) %>% 
    filter(
        debut > '1965-01-01',
        marks >= 673
    ) %>% nrow()

#tackles
afltables %>% group_by(id, first_name, surname) %>% 
    summarise(
        tackles = sum(tackles),
        debut = min(date),
        # mrg = max(date)
    ) %>% arrange(-tackles) %>% 
    filter(
        debut > '1965-01-01',
        tackles >= 407
    ) %>% nrow()


