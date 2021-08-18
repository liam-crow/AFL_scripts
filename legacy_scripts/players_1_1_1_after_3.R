afltables %>% 
    select(season, round, date, id, first_name, surname, w_l, playing_for, opp) %>% 
    group_by(id, first_name, surname) %>% arrange(date) %>% 
    mutate(games_played = row_number(), debut = min(date)) %>% ungroup() %>% 
    filter(games_played %in% c(1,2,3), debut >= '1965-01-01') %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        wdl = length(unique(w_l)),
        wdl_order = paste(w_l, collapse = ', ')
    ) %>% filter(wdl == 3) %>%
    group_by(wdl_order) %>% 
    summarise(
        n = n(),
        players = paste(first_name, surname, collapse = ', ')
    ) %>% View()

afltables %>% 
    select(season, round, date, id, first_name, surname, w_l, playing_for, opp) %>% 
    group_by(id, first_name, surname) %>% arrange(date) %>% 
    mutate(games_played = row_number(), debut = min(date)) %>% ungroup() %>% 
    filter(games_played %in% c(1,2,3,4,5,6)) %>% 
    group_by(debut, id, first_name, surname, w_l) %>% 
    count() %>% ungroup() %>% filter(n == 2) %>% 
    group_by(debut, id, first_name, surname) %>% 
    count() %>% ungroup() %>% arrange(-n, desc(debut))

afltables %>% 
    select(season, round, date, id, first_name, surname, w_l, playing_for, opp) %>% 
    group_by(id, first_name, surname) %>% arrange(date) %>% 
    mutate(games_played = row_number(), debut = min(date)) %>% ungroup() %>% 
    filter(games_played %in% c(1,2,3,4,5,6,7,8,9)) %>% 
    group_by(debut, id, first_name, surname, w_l) %>% 
    count() %>% ungroup() %>% filter(n == 3) %>% 
    group_by(debut, id, first_name, surname) %>% 
    count() %>% ungroup() %>% arrange(-n, desc(debut))
