
afltables %>% 
    select(
        season, round, date, playing_for, opp,
        pq_1_g, pq_1_b, oq_1_g, oq_1_b, playing_for_score, opp_score
    ) %>% distinct() %>% 
    mutate(
        pq_1_total = pq_1_g*6 + pq_1_b,
        oq_1_total = oq_1_g*6 + oq_1_b,
        diff_final = playing_for_score - opp_score,
        diff_q1 = pq_1_total - oq_1_total
    ) %>% 
    filter(diff_final > 0, diff_q1 > 0) %>% 
    mutate(main_diff = diff_final - diff_q1) %>% View()
    
