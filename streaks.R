library(dplyr)

afltables %>% 
    select(season, round, date, id, first_name, surname) %>% 
    mutate(not_thurs = weekdays(date) != "Thursday") %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(streak_id = cumsum(not_thurs)) %>% 
    group_by(id, first_name, surname, streak_id) %>% 
    mutate(streak = row_number()-1) %>% ungroup()

afltables %>% 
    filter(season >= 1994) %>% 
    select(season, round, date, id, first_name, surname, tackles) %>% 
    mutate(tf_tackles = tackles != 0) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(streak_id = cumsum(tf_tackles)) %>% 
    group_by(id, first_name, surname, streak_id) %>% 
    mutate(streak = row_number()-1) %>% ungroup() %>% View()

afltables %>% 
    filter(season >= 1994) %>% 
    select(season, round, date, id, first_name, surname, disposals, tackles) %>% 
    mutate(tf = !(disposals >=20 & tackles == 0)) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(streak_id = cumsum(tf)) %>% 
    group_by(id, first_name, surname, streak_id) %>% 
    mutate(streak = row_number()-1) %>% ungroup() %>% View()

afltables %>% 
    filter(season >= 1965) %>% 
    select(season, round, date, id, first_name, surname, disposals) %>% 
    mutate(tf = !(disposals == 0)) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(streak_id = cumsum(tf)) %>% 
    group_by(id, first_name, surname, streak_id) %>% 
    mutate(streak = row_number()-1) %>% ungroup() %>% View()

afltables %>% 
    filter(season >= 1999) %>% 
    select(season, round, date, id, first_name, surname, bounces) %>% distinct() %>% 
    mutate(tf = !(bounces == 0)) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(streak_id = cumsum(tf)) %>% 
    group_by(id, first_name, surname, streak_id) %>% 
    mutate(streak = row_number()-1) %>% ungroup() %>% View()

afltables %>% 
    select(season, round, date, id, first_name, surname, marks) %>% 
    filter(id == '11743') %>% arrange(date) %>% View()

afltables %>% 
    select(season, round, date, id, first_name, surname, playing_for_score) %>% distinct() %>% 
    mutate(tf = !(is_prime(playing_for_score))) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(streak_id = cumsum(tf)) %>% 
    group_by(id, first_name, surname, streak_id) %>% 
    mutate(streak = row_number()-1) %>% ungroup() %>% View()

afltables %>% distinct(id, first_name, surname) %>% View()
    
afltables %>% filter(surname == 'Primus') %>% 
    select(id, first_name, surname, playing_for_score) %>% 
    mutate(is_prime = is_prime(playing_for_score)) %>% 
    group_by(is_prime) %>% count()
