library(dplyr)

teams_exp <- afltables %>% 
    select(season, round, date, home_team, away_team, playing_for, id, first_name, surname, w_l) %>% 
    group_by(id) %>% arrange(date) %>% 
    mutate(game_no = row_number()) %>% ungroup() %>% 
    group_by(season, round, date, playing_for, w_l) %>% 
    summarise(
        total_exp = sum(game_no),
        .groups = 'drop'
    ) 

teams_exp %>% 
    group_by(playing_for) %>% 
    arrange(date) %>% 
    mutate(
        exp_diff = total_exp - lag(total_exp)
    ) %>% View()
