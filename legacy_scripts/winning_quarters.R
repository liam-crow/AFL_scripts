
library(dplyr)

afltables_Q <- afltables %>% 
    select(season, round, playing_for, opp, starts_with('pq'), starts_with('oq'), playing_for_score, opp_score) %>% distinct() %>% 
    mutate(
        q1_W = pq_1_g*6 + pq_1_b > oq_1_g*6 + oq_1_b,
        q2_W =  (pq_2_g*6 + pq_2_b) - (pq_1_g*6 + pq_1_b) > oq_2_g*6 + oq_2_b - (oq_1_g*6 + oq_1_b),
        q3_W =  (pq_3_g*6 + pq_3_b) - (pq_2_g*6 + pq_2_b) > oq_3_g*6 + oq_3_b - (oq_2_g*6 + oq_2_b),
        q4_W =  (pq_4_g*6 + pq_4_b) - (pq_3_g*6 + pq_3_b) > oq_4_g*6 + oq_4_b - (oq_3_g*6 + oq_3_b),
        TQ_W = q1_W + q2_W + q3_W + q4_W,
        margin = playing_for_score - opp_score
    ) %>% select(-starts_with('pq'), -starts_with('oq')) #%>% 

afltables_Q %>% filter(TQ_W == 1) %>% 
    arrange(-margin) %>% View()

afltables_Q %>% View()
