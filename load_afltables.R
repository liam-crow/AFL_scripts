library(fitzRoy)
library(dplyr)

afltables <- fitzRoy::fetch_player_stats_afltables()
names(afltables) <- snakecase::to_snake_case(names(afltables))

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

afltables <- afltables %>% 
    filter(playing_for == home_team | playing_for == away_team) %>% distinct()
    

# add disposals, 
# home/away, win/loss/draw per player

afltables <- afltables %>% 
    mutate(
        jumper_no = as.numeric(gsub('[^0-9]','', jumper_no)),
        disposals = kicks + handballs,
        
        playing_for_short = case_when(
            playing_for == 'Carlton' ~ 'Carl',
            playing_for == 'Fitzroy' ~ 'Fitz',
            playing_for == 'Sydney' ~ 'Syd',
            playing_for == 'Essendon' ~ 'Ess',
            playing_for == 'Geelong' ~ 'Geel',
            playing_for == 'Melbourne' ~ 'Melb',
            playing_for == 'St Kilda' ~ 'StK',
            playing_for == 'Collingwood' ~ 'Coll',
            playing_for == 'Adelaide' ~ 'Ade',
            playing_for == 'Hawthorn' ~ 'Haw',
            playing_for == 'West Coast' ~ 'WCE',
            playing_for == 'Western Bulldogs' ~ 'WB',
            playing_for == 'North Melbourne' ~ 'NthM',
            playing_for == 'Richmond' ~ 'Rich',
            playing_for == 'Brisbane Bears' ~ 'BB',
            playing_for == 'Fremantle' ~ 'Frem',
            playing_for == 'Brisbane Lions' ~ 'BL',
            playing_for == 'Port Adelaide' ~ 'PA',
            playing_for == 'Gold Coast' ~ 'GC',
            playing_for == 'Greater Western Sydney' ~ 'GWS',
            playing_for == 'University' ~ 'Uni',
            T ~ playing_for
        ),
        
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
    ) %>%
    mutate(
        possessions = contested_possessions + uncontested_possessions,
        fantasy_points = kicks*3 + 
            handballs*2 +
            marks*3 +
            tackles*4 +
            frees_for + 
            frees_against*-3 +
            hit_outs +
            goals*6 +
            behinds
    ) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(games_played = row_number()) %>% ungroup()
    

#### Run length encoding (Consecutive) functions
# USE THIS ONE
rle_tbl <- function(col) {
    rle_res <- rle(col)
    data_frame(
        len = rle_res$lengths,
        val = rle_res$values
    )
}

## NOT THESE (here for legacy reasons)
# rle_len_calc <- function(col) {
#     max(rle(col)$lengths)
# }
# 
# rle_val_calc <- function(col) {
#     rle_out <- rle(col)
#     out <- tibble(
#         len = rle_out$lengths,
#         val = rle_out$values
#     ) %>% 
#         filter(len == max(len)) %>% 
#         pull(val)
#     paste(out, collapse = ', ')
# }

# rle_len_calc(c(T,F,T,T,T,F,T,T))
# rle_val_calc(c(T,F,T,T,T,F,T,T))
# 
# rle(c(T,F,T,T,T,F,T,T))$lengths

## Example use
# consecutive_results <- afltables %>% 
#     filter(season > 1964) %>% 
#     select(season, date, id, first_name, surname, disposals, goals) %>% 
#     group_by(id, first_name, surname) %>% 
#     arrange(date) %>% 
#     mutate(
#         diff_from_prev_disp = disposals - lag(disposals),
#         diff_disp_tf = if_else(between(diff_from_prev_disp, -1, 1), T, F)
#     ) %>%
#     nest(-id, -first_name, -surname) %>% 
#     mutate(
#         rle_res = map(data, ~rle_tbl(col = .x$diff_disp_tf))
#     ) %>% 
#     unnest(rle_res)
