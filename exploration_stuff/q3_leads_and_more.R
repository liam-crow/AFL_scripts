library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
source("load_afltables.R")

library(ggplot2)
library(ggdark)
library(Cairo)

#### q3 leads ####
q3_leads <- afltables %>% 
    select(season, date, round, playing_for_short, opp_short, pq_3_g, pq_3_b, oq_3_g, oq_3_b, playing_for_score, opp_score, w_l) %>% 
    distinct() %>% 
    mutate(
        game_id = row_number(),
        q3_margin = pq_3_g*6 + pq_3_b - (oq_3_g*6 + oq_3_b),
        q4_margin = playing_for_score - opp_score
    ) %>% 
    filter(q3_margin > 0) %>% 
    pivot_longer(
        cols = c('q3_margin', 'q4_margin'),
        names_to = "quarter",
        values_to = "margin"
    ) %>% 
    mutate(
        col_true = if_else(playing_for_short == "GEEL" & season == '1990' & w_l == 'L',T,F),
        quarter = case_when(
            quarter == 'q3_margin' ~ 'Q3',
            quarter == 'q4_margin' ~ 'Q4',
            T ~ quarter
        )
    )

q3_leads %>% 
    group_by(w_l) %>% count()

plot <- ggplot(q3_leads, aes(x = quarter, y = margin)) + 
    geom_point(fill = 'grey', colour = 'grey', alpha = .1) + 
    geom_line(aes(group = game_id), colour = 'grey', size = 0.25, alpha = .1) +
    geom_point(
        q3_leads %>% filter(col_true == T),
        mapping = aes(x = quarter, y = margin),
        colour = 'green'
    ) + 
    geom_line(
        q3_leads %>% filter(col_true == T),
        mapping = aes(group = game_id), colour = 'green', size = 1, alpha = 0.7
    ) +
    xlab('') + ylab('Margin') +
    ggtitle('All Teams Leading at Q3, 1897-2021',
            subtitle = 'Highlighted Geelong 1990, 7 Losses from leading at Q3\nThe most in a V/AFL season, Useless AFL Stats') +
    ggdark::dark_theme_classic() +
    theme(text = element_text(size = 15))

ggsave(plot, filename = 'exploration_stuff/geel_1990.png', dpi = 300, type = 'cairo',
       width = 20, height = 15, units = 'cm')


#### first 3 leads ####
q1_to_3_leads <- afltables %>% 
    select(season, date, round, playing_for_short, opp_short, h_a,
           pq_1_g, pq_1_b, oq_1_g, oq_1_b,  pq_2_g, pq_2_b, oq_2_g, oq_2_b, pq_3_g, pq_3_b, oq_3_g, oq_3_b, 
           playing_for_score, opp_score, w_l) %>% 
    distinct() %>% 
    mutate(
        game_id = row_number(),
        q1_margin = pq_1_g*6 + pq_1_b - (oq_1_g*6 + oq_1_b),
        q2_margin = pq_2_g*6 + pq_2_b - (oq_2_g*6 + oq_2_b),
        q3_margin = pq_3_g*6 + pq_3_b - (oq_3_g*6 + oq_3_b),
        q4_margin = playing_for_score - opp_score,
        .keep = 'unused'
    ) %>% 
    # filter(h_a == 'H', w_l == 'D') %>% 
    filter(q1_margin >= 30) %>% 
    pivot_longer(
        cols = c('q1_margin', 'q2_margin','q3_margin', 'q4_margin'),
        names_to = "quarter",
        values_to = "margin"
    ) %>% 
    mutate(
        # col_true = if_else(playing_for_short == "GEEL" & season == '1990' & w_l == 'L',T,F),
        quarter = case_when(
            quarter == 'q1_margin' ~ 'Q1',
            quarter == 'q2_margin' ~ 'Q2',
            quarter == 'q3_margin' ~ 'Q3',
            quarter == 'q4_margin' ~ 'Q4',
            T ~ quarter
        )
    )

plot <- ggplot(q1_to_3_leads, aes(x = quarter, y = margin)) + 
    geom_point(fill = 'grey', colour = 'grey', alpha = .1) + 
    geom_line(aes(group = game_id), colour = 'grey', size = 0.25, alpha = .1) +
    # geom_point(
    #     q3_leads %>% filter(col_true == T),
    #     mapping = aes(x = quarter, y = margin),
    #     colour = 'green'
    # ) + 
    # geom_line(
    #     q3_leads %>% filter(col_true == T),
    #     mapping = aes(group = game_id), colour = 'green', size = 1, alpha = 0.7
    # ) +
    xlab('') + ylab('Margin') +
    ggtitle('All V/AFL games with a team leading by 30+ at Q1',
            subtitle = '90% of teams go on to win. @crow_data_sci, data from #fitzRoy') +
    ggdark::dark_theme_classic() +
    theme(text = element_text(size = 8))

q1_to_3_leads %>% 
    group_by(w_l) %>% count()

ggsave(plot, filename = 'exploration_stuff/lead_30.png', dpi = 300, type = 'cairo',
       width = 16, height = 9, units = 'cm')
