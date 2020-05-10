library(fitzRoy)
library(dplyr)

afltables <- fitzRoy::get_afltables_stats()
names(afltables) <- to_snake_case(names(afltables))

# fix cam raynor stats
afltables <- afltables %>%
    mutate(
        id = case_when(
            id == 0 ~ 12592,
            TRUE ~ id
        ),
        first_name = case_when(
            id == "12592" ~ "Cam",
            TRUE ~ first_name
        ),
        surname = case_when(
            id == "12592" ~ "Rayner",
            TRUE ~ surname
        ),
        playing_for = case_when(
            id == "12592" ~ "Brisbane Lions",
            TRUE ~ playing_for
        )
    )

# add disposals, 
# home/away, win/loss/draw per player

afltables <- afltables %>% 
    mutate(
        disposals = kicks + handballs,
        
        playing_for_score = case_when(
            playing_for == home_team ~ home_score,
            TRUE ~ away_score
        ),
        opp = case_when(
            playing_for == home_team ~ away_team,
            TRUE ~ home_team
        ),
        opp_score = case_when(
            playing_for == home_team ~ away_score,
            TRUE ~ home_score
        ),
        h_a = case_when(
            playing_for == home_team ~ 'H',
            TRUE ~ 'A'
        ),
        w_l = case_when(
            playing_for == home_team & home_score > away_score ~ 'W',
            playing_for == home_team & home_score < away_score ~ 'L',
            playing_for == away_team & away_score > home_score ~ 'W',
            playing_for == away_team & away_score < home_score ~ 'L',
            TRUE ~ 'D'
        ),
        pq_1_g = if_else(playing_for == home_team, hq_1_g, aq_1_g),
        pq_1_b = if_else(playing_for == home_team, hq_1_b, aq_1_b),
        pq_2_g = if_else(playing_for == home_team, hq_2_g, aq_2_g),
        pq_2_b = if_else(playing_for == home_team, hq_2_b, aq_2_b),
        pq_3_g = if_else(playing_for == home_team, hq_3_g, aq_3_g),
        pq_3_b = if_else(playing_for == home_team, hq_3_b, aq_3_b),
        pq_4_g = if_else(playing_for == home_team, hq_4_g, aq_4_g),
        pq_4_b = if_else(playing_for == home_team, hq_4_b, aq_4_b),
        
        oq_1_g = if_else(playing_for == home_team, aq_1_g, hq_1_g),
        oq_1_b = if_else(playing_for == home_team, aq_1_b, hq_1_b),
        oq_2_g = if_else(playing_for == home_team, aq_2_g, hq_2_g),
        oq_2_b = if_else(playing_for == home_team, aq_2_b, hq_2_b),
        oq_3_g = if_else(playing_for == home_team, aq_3_g, hq_3_g),
        oq_3_b = if_else(playing_for == home_team, aq_3_b, hq_3_b),
        oq_4_g = if_else(playing_for == home_team, aq_4_g, hq_4_g),
        oq_4_b = if_else(playing_for == home_team, aq_4_b, hq_4_b)
    ) 
