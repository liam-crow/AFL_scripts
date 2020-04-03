
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- fitzRoy::get_afltables_stats(start_date = "1900-01-01")
names(afltables) <- snakecase::to_snake_case(names(afltables))

brownlow_3votes <- afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname, brownlow_votes) %>% 
    filter(
        date > "1980-01-01",
        brownlow_votes %in% 1:3
    )

id_3votes_once <- brownlow_3votes %>% 
    group_by(id) %>% summarise(sum = sum(brownlow_votes))

brownlow_3votes %>% inner_join(id_3votes_once, by = 'id') %>% 
    filter(
        round == 1,
        brownlow_votes == 3,
        sum == 3
    )



