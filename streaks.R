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
    select(season, round, date, id, first_name, surname, marks) %>% distinct() %>% 
    mutate(tf = !(marks == 5)) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(streak_id = cumsum(tf)) %>% 
    group_by(id, first_name, surname, streak_id) %>% 
    mutate(streak = row_number()-1) %>% ungroup() %>% View()

afltables %>% 
    select(season, round, date, playing_for, playing_for_score, pq_3_g, pq_3_b, w_l) %>% distinct() %>% 
    mutate(pq_3_score = pq_3_g*6 + pq_3_b) %>% 
    mutate(tf = !(playing_for_score/2 >= pq_3_score & w_l == 'W')) %>% 
    group_by(playing_for) %>% 
    arrange(date) %>% 
    mutate(streak_id = cumsum(tf)) %>% 
    group_by(playing_for, streak_id) %>% 
    mutate(streak = row_number()-1) %>% ungroup() %>% View()

afltables %>% 
    select(season, round, date, id, first_name, surname, marks) %>% 
    filter(id == '11743') %>% arrange(date) %>% View()

afltables %>% 
    select(season, round, date, id, first_name, surname, playing_for_score) %>% distinct() %>% 
    mutate(tf = !(sjstats::is_prime(playing_for_score))) %>% 
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

afltables %>% 
    select(season, round, date, playing_for, pq_4_g, pq_4_b) %>% distinct() %>% 
    mutate(tf = !(pq_4_g == pq_4_b)) %>% 
    group_by(playing_for) %>% 
    arrange(date) %>% 
    mutate(streak_id = cumsum(tf)) %>% 
    group_by(playing_for, streak_id) %>% 
    mutate(streak = row_number()-1) %>% ungroup() %>% View()
