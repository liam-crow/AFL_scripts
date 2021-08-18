library(dplyr)

afltables %>% 
    select(
        season, round, date, home_team, away_team, playing_for, starts_with('pq_'), playing_for_score
    ) %>% distinct() %>% 
    mutate(q2_comb = as.numeric(paste0(pq_2_g, pq_2_b))) %>% 
    filter(playing_for_score == q4_comb) %>% 
    group_by(season, round, date, home_team, away_team) %>% 
    count() %>% arrange(-n)
