
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

library(ggplot2)
library(ggpubr)
library(ggsci)
library(forcats)
library(viridis)
library(patchwork)
library(ggthemes)

theme_set(
    theme_economist()
)

#### All teams ####
q_diff_season_team <- afltables %>% 
    mutate(
        Q4 = (pq_4_g - pq_3_g)*6 + pq_4_b - pq_3_b,
        Q3 = (pq_3_g - pq_2_g)*6 + pq_3_b - pq_2_b,
        Q2 = (pq_2_g - pq_1_g)*6 + pq_2_b - pq_1_b,
        Q1 = pq_1_g*6 + pq_1_b
    ) %>% 
    select(season, round, home_team, away_team, playing_for, starts_with('Q')) %>% #distinct %>% View()
    distinct() %>% 
    pivot_longer(
        cols = starts_with("Q"),
        names_to  = 'q_diff',
        values_to = 'diff_val'
    ) %>% 
    group_by(season, q_diff) %>% 
    summarise(avg_diff = mean(diff_val))

p1 <- ggplot(
    q_diff_season_team %>% filter(season < 2020), # remove 2020 season
    aes(x = season, y = avg_diff, colour = q_diff)
) + 
    geom_line() + 
    geom_smooth(method = 'loess', span = 0.2, se = F) +
    theme(
        legend.position = 'right',
        legend.title = element_blank()
    ) + 
    ggtitle('Average Points Scored per Team per Quarter', subtitle = 'Not including 2020 season') + 
    guides(color = guide_legend(reverse = TRUE)) +
    xlab('Season') + ylab('') +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 41)) +
    scale_x_continuous(expand = c(0, 0), limits = c(1890, 2025), breaks = 190:202*10) +
    scale_colour_excel_new()

#### Richmond ####
q_diff_season_team <- afltables %>% 
    filter(playing_for == 'Richmond') %>%
    mutate(
        Q4 = (pq_4_g - pq_3_g)*6 + pq_4_b - pq_3_b,
        Q3 = (pq_3_g - pq_2_g)*6 + pq_3_b - pq_2_b,
        Q2 = (pq_2_g - pq_1_g)*6 + pq_2_b - pq_1_b,
        Q1 = pq_1_g*6 + pq_1_b
    ) %>% 
    select(season, round, home_team, away_team, playing_for, starts_with('Q')) %>% distinct %>% View()
    distinct() %>% 
    pivot_longer(
        cols = starts_with("Q"),
        names_to  = 'q_diff',
        values_to = 'diff_val'
    ) %>% 
    group_by(season, q_diff) %>% 
    summarise(avg_diff = mean(diff_val))

p2 <- ggplot(
    q_diff_season_team %>% filter(season < 2020), # remove 2020 season
    aes(x = season, y = avg_diff, colour = q_diff)
) + 
    geom_line() + 
    geom_smooth(method = 'loess', span = 0.2, se = F) +
    theme(
        legend.position = 'right',
        legend.title = element_blank()
    ) + 
    ggtitle('Average Points Scored per Quarter, Richmond', subtitle = 'Not including 2020 season') + 
    guides(color = guide_legend(reverse = TRUE)) +
    xlab('Season') + ylab('') +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 40)) +
    scale_x_continuous(expand = c(0, 0), limits = c(1890, 2025), breaks = 190:202*10) +
    scale_colour_excel_new()


#### Geelong ####
q_diff_season_team <- afltables %>% 
    filter(playing_for == 'Geelong') %>%
    mutate(
        Q4 = (pq_4_g - pq_3_g)*6 + pq_4_b - pq_3_b,
        Q3 = (pq_3_g - pq_2_g)*6 + pq_3_b - pq_2_b,
        Q2 = (pq_2_g - pq_1_g)*6 + pq_2_b - pq_1_b,
        Q1 = pq_1_g*6 + pq_1_b
    ) %>% 
    select(season, round, home_team, away_team, starts_with('Q')) %>% #distinct %>% View()
    distinct() %>% 
    pivot_longer(
        cols = starts_with("Q"),
        names_to  = 'q_diff',
        values_to = 'diff_val'
    ) %>% 
    group_by(season, q_diff) %>% 
    summarise(avg_diff = mean(diff_val))

p3 <- ggplot(
    q_diff_season_team %>% filter(season < 2020), # remove 2020 season
    aes(x = season, y = avg_diff, colour = q_diff)
) + 
    geom_line() + 
    geom_smooth(method = 'loess', span = 0.2, se = F) +
    theme(
        legend.position = 'right',
        legend.title = element_blank()
    ) + 
    ggtitle('Average Points Scored per Quarter, Geelong', subtitle = 'Not including 2020 season') + 
    guides(color = guide_legend(reverse = TRUE)) +
    xlab('Season') + ylab('') +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 40)) +
    scale_x_continuous(expand = c(0, 0), limits = c(1890, 2025), breaks = 190:202*10) +
    scale_colour_excel_new()

#### Melbourne ####
q_diff_season_team <- afltables %>% 
    filter(playing_for == 'Melbourne') %>%
    mutate(
        Q4 = (pq_4_g - pq_3_g)*6 + pq_4_b - pq_3_b,
        Q3 = (pq_3_g - pq_2_g)*6 + pq_3_b - pq_2_b,
        Q2 = (pq_2_g - pq_1_g)*6 + pq_2_b - pq_1_b,
        Q1 = pq_1_g*6 + pq_1_b
    ) %>% 
    select(season, round, home_team, away_team, starts_with('Q')) %>% #distinct %>% View()
    distinct() %>% 
    pivot_longer(
        cols = starts_with("Q"),
        names_to  = 'q_diff',
        values_to = 'diff_val'
    ) %>% 
    group_by(season, q_diff) %>% 
    summarise(avg_diff = mean(diff_val))

p4 <- ggplot(
    q_diff_season_team %>% filter(season < 2020), # remove 2020 season
    aes(x = season, y = avg_diff, colour = q_diff)
) + 
    geom_line() + 
    geom_smooth(method = 'loess', span = 0.2, se = F) +
    theme(
        legend.position = 'right',
        legend.title = element_blank()
    ) + 
    ggtitle('Average Points Scored per Quarter, Melbourne', subtitle = 'Not including 2020 season') + 
    guides(color = guide_legend(reverse = TRUE)) +
    xlab('Season') + ylab('') +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 40)) +
    scale_x_continuous(expand = c(0, 0), limits = c(1890, 2025), breaks = 190:202*10) +
    scale_colour_excel_new()

#### all together now ####
(p1 + p2)/(p3 + p4)

