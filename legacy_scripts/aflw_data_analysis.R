
library(dplyr)
library(fitzRoy)

cookie <- get_aflw_cookie()

match_data <- get_aflw_match_data()
names(match_data) <- snakecase::to_snake_case(names(match_data))

glimpse(match_data)

View(match_data)

aflw_match_ids <- match_data %>% pull(match_id)

aflw_detailed <- get_aflw_detailed_data(aflw_match_ids)

View(aflw_detailed)

aflw_player_stats <- get_aflw_player_stats()

aflw_player_stats %>% 
    group_by(player_id, player_name) %>% 
    summarise(
        s_kicks = sum(kicks),
        s_hball = sum(handballs),
        s_disp = sum(disposals),
    ) %>% View()

aflw_player_stats %>% 
    select(date, fixture_round, team, player_id, player_name, number, clangers, disposals, kicks, handballs, goals, behinds, marks, bounces, hitouts, total_clearances) %>% 
    filter(number == clangers) %>% View()
