
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- fitzRoy::get_afltables_stats(start_date = "1900-01-01")
names(afltables) <- snakecase::to_snake_case(names(afltables))

dates <- timeDate::Easter(year = c(1965:2019))
dates <- as.character(dates)

afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname, kicks, handballs) %>% 
    mutate(date = as.character(date)) %>% 
    filter(date %in% dates) %>% 
    mutate(
        disp = kicks + handballs
    ) %>% 
    filter(disp == 0)
