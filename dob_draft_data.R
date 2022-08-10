
library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
library(stringr)
library(ggplot2)
source("load_afltables.R")
source("load_fryzigg.R")

player_details <- NULL

for (i in 2013:2022) {
    player_i <- fitzRoy::fetch_player_details_afl(season = i) %>% select(-one_of('position'))
    print(dim(player_i))
    player_details <- rbind(player_details,player_i)
}

player_dob <- player_details %>% 
    rename_with(snakecase::to_snake_case) %>% 
    select(id, first_name, surname, playing_for = team, date_of_birth, draft_year, draft_position) %>% 
    distinct() %>% 
    mutate(
        playing_for = case_when(
            playing_for == 'Adelaide Crows' ~ 'Adelaide',
            playing_for == 'Gold Coast Suns' ~ 'Gold Coast',
            playing_for == 'Geelong Cats' ~ 'Geelong',
            playing_for == 'Sydney Swans' ~ 'Sydney',
            playing_for == 'GWS Giants' ~ 'Greater Western Sydney',
            playing_for == 'West Coast Eagles' ~ 'West Coast',
            T ~ playing_for
        )
    )

player_ratings <- fryzigg_data %>% 
    filter(season >= 2013) %>% 
    select(id, first_name, surname, season, date, round, playing_for, rating_points)

comb <- player_ratings %>% 
    left_join(player_dob, by = c('first_name','surname','playing_for')) %>% 
    mutate(
        age = as.numeric(as.Date(date) - as.Date(date_of_birth))/364,
        draft_position = as.numeric(draft_position)
    ) %>% 
    drop_na()

# comb %>% 
#     filter(is.na(date_of_birth)) %>% 
#     distinct(id.x, first_name, surname) %>% View()
comb %>% 
    # filter(playing_for == 'Port Adelaide') %>% 
    ggplot(aes(x = age, y = rating_points)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~draft_position, scales = 'free_y')
    
