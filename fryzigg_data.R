library(dplyr)
library(lubridate)

fryzigg_data <- as_tibble(fitzRoy::get_fryzigg_stats(start = '2000')) %>% 
    mutate(
        date = as.Date(match_date),
        season = year(date),
        local_start_time = hms(match_local_time),
        .before = match_date
    )

# fryzigg_2020 %>% 
#     group_by(match_round, player_team) %>% 
#     summarise(
#         team_weight = sum(player_weight_kg),
#         team_weight_avg = mean(player_weight_kg),
#         .groups = 'drop'
#     ) %>% 
#     mutate(match_round = as.numeric(match_round)) %>% 
#     group_by(player_team) %>% 
#     arrange(match_round) %>% 
#     mutate(
#         diff_t = team_weight - lag(team_weight),
#         diff_a = team_weight_avg - lag(team_weight_avg),
#     ) %>% View()
