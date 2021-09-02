library(dplyr)
library(lubridate)

fryzigg_data <- as_tibble(fitzRoy::fetch_player_stats(2000:2030, comp = 'AFLM', source = 'fryzigg')) %>% 
    select(-match_id, -match_date) %>% 
    rename_with(~gsub('^match_|^player_','',.x)) %>% 
    rename(
        surname = last_name,
        playing_for = team,
        frees_for = free_kicks_for,
        frees_against = free_kicks_against,
        hit_outs = hitouts
    ) %>% 
    mutate(
        season = year(date),
        local_start_time = hms(local_time),
        fantasy_points = kicks*3 + 
            handballs*2 +
            marks*3 +
            tackles*4 +
            frees_for + 
            frees_against*-3 +
            hit_outs +
            goals*6 +
            behinds
    )
