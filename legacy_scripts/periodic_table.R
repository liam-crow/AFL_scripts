
periodic_table_data <- afltables %>% 
    group_by(id, first_name, surname) %>% 
    mutate(
        total_games = max(row_number()),
        total_goals = sum(goals)
    ) %>% 
    group_by(playing_for, id, first_name, surname, total_games, total_goals) %>% 
    summarise(
        j_no = paste(unique(jumper_no), collapse = ', '),
        club_games = max(row_number())
    ) %>% ungroup() %>% 
    mutate(
        init_1 = strtrim(first_name, 1),
        init_2 = strtrim(surname, 1),
        sur_2 = strtrim(surname, 2),
    ) %>% 
    arrange(-club_games)

write.csv(periodic_table_data, "periodic_table_data.csv", row.names = F)
