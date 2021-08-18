
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)
library(fitzRoy)

source("load_afltables.R")

fp_margin <- afltables %>% 
    select(
        season, round, date, home_team, away_team, id, first_name, surname, playing_for, playing_for_score, opp_score,
        kicks, handballs, marks, tackles, frees_for, frees_against, hit_outs, goals, behinds   
    ) %>% 
    mutate(
        margin = playing_for_score - opp_score,
        fantasy_points = kicks*3 + handballs*2 + marks*3 + tackles*4 + frees_for +
            frees_against*-3 + hit_outs + goals*6 + behinds
    ) %>% distinct() %>% 
    filter(season > 2000) %>% 
    group_by(id) %>% mutate(games_played = n()) %>% ungroup()

library(broom)
library(purrr)

fp_margin_mod_out <- fp_margin %>% 
    filter(games_played >= 50) %>% 
    select(id, first_name, surname, fantasy_points, margin) %>% 
    nest(-id, -first_name, -surname) %>% 
    mutate(
        fit = map(data, ~ lm(fantasy_points ~ margin, data = .x)),
        tidied = map(fit, tidy)
    ) %>% 
    unnest(tidied)

fp_margin_mod_out %>% 
    filter(term == 'margin') %>% 
    filter(
        estimate == min(estimate) | estimate == max(estimate) | abs(estimate) == min(abs(estimate))
    ) %>% arrange(estimate)

fp_margin_player <- fp_margin %>% 
    filter(id == '4042')

library(ggplot2)
# Generic Testing one
ggplot(fp_margin_player, aes(y = fantasy_points, x = margin)) + 
    geom_point() + 
    geom_smooth(method = 'loess')

# 
p1 <- ggplot(fp_margin %>% filter(id == '11713'), aes(y = fantasy_points, x = margin)) + 
    geom_point() + 
    geom_smooth(method = 'loess') +
    ggtitle('Taylor Walker') +
    xlim(-115,125) + ylim(5,170) + xlab('Margin') + ylab('Fantasy Points')

p2 <- ggplot(fp_margin %>% filter(id == '11787'), aes(y = fantasy_points, x = margin)) + 
    geom_point() + 
    geom_smooth(method = 'loess') +
    ggtitle('Tom Rockliff') +
    xlim(-115,125) + ylim(5,170) + xlab('Margin') + ylab('Fantasy Points')

p3 <- ggplot(fp_margin %>% filter(id == '11733'), aes(y = fantasy_points, x = margin)) + 
    geom_point() + 
    geom_smooth(method = 'loess') +
    ggtitle('Alex Rance') +
    xlim(-115,125) + ylim(5,170) + xlab('Margin') + ylab('Fantasy Points')

p4 <- ggplot(fp_margin %>% filter(id == '11538'), aes(y = fantasy_points, x = margin)) + 
    geom_point() + 
    geom_smooth(method = 'loess') +
    ggtitle('Scott Selwood') +
    xlim(-115,125) + ylim(5,170) + xlab('Margin') + ylab('Fantasy Points')

library(patchwork)

(p1+p2)/(p3+p4)
