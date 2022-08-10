library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
source("load_fryzigg.R") #runs script that loads in data automatically and cleans

library(ggplot2)
library(ggrepel)
library(patchwork)

team_colours <- c(
    'Carlton'= '#0e1e2d',
    'Geelong'= '#1c3c63',
    'Adelaide'= '#002b5c',
    'Sydney' = '#ed171f',
    'St Kilda' = '#ed0f05',
    'Essendon' = '#cc2031',
    'Richmond'= '#D8D800',
    'Melbourne'= '#0f1131',
    'Hawthorn' = '#4d2004',
    'Brisbane Lions' = '#a30046',
    'Brisbane Bears' = '#a30046',
    'Fitzroy' = '#d93e39',
    'Gold Coast' = '#d93e39',
    'Fremantle'= '#2a1a54',
    'Collingwood'= '#000000',
    'Port Adelaide'  = '#01b5b6',
    'Western Bulldogs'  = '#014896',
    'Footscray'  = '#014896',
    'West Coast' = '#062ee2',
    'Greater Western Sydney' = '#f15c22',
    'North Melbourne'= '#013b9f'
)

#### defensive 1v1s ####
season_choice = c(2021,2022)

def_1v1_data <- fryzigg_data %>% 
    select(date, season, round, playing_for, contest_def_one_on_ones, contest_def_losses, height_cm) %>% 
    drop_na() %>%
    filter(season %in% season_choice, round %in% c(1:25)) %>% 
    group_by(season, playing_for) %>% 
    summarise(
        n = length(unique(round)),
        t_def_one_on_ones = sum(contest_def_one_on_ones)/n,
        t_def_one_on_ones_losses = sum(contest_def_losses)/n,
        loss_ratio = round(t_def_one_on_ones_losses/t_def_one_on_ones*100,2),
        .groups = 'drop'
    )

p1 <- ggplot(def_1v1_data, aes(x = t_def_one_on_ones, y = loss_ratio, colour = playing_for)) + 
    geom_point() +
    scale_colour_manual(values = team_colours) +
    geom_text_repel(aes(label = playing_for),size = 2) +
    facet_grid(~season) +
    labs(title = 'AFL Defensive 1v1 Team Averages (2021-2022)', caption = '@crow_data_sci') +
    xlab('Average Defensive 1v1s per game')+
    ylab('Percentage Lost') +
    theme(
        legend.position = 'none'
    )

def_1v1_data_ind <- fryzigg_data %>% 
    select(date, season, round, playing_for, id, first_name, surname, contest_def_one_on_ones, contest_def_losses, height_cm) %>% 
    drop_na() %>% 
    # filter(height_cm <= 185) %>% 
    filter(season %in% season_choice, round %in% c(1:25), playing_for == 'Essendon') %>% 
    group_by(season, playing_for, id, first_name, surname) %>% 
    summarise(
        n = length(unique(round)),
        t_def_one_on_ones = sum(contest_def_one_on_ones)/n,
        t_def_one_on_ones_losses = sum(contest_def_losses)/n,
        loss_ratio = round(t_def_one_on_ones_losses/t_def_one_on_ones*100,2),
        .groups = 'drop'
    ) %>% 
    # filter(n >= 5) %>% 
    arrange(desc(t_def_one_on_ones)) %>% 
    group_by(season) %>% 
    slice(1:10) %>% 
    rowwise() %>% 
    mutate(
        name = paste(strsplit(first_name,'')[[1]][1], surname)
    )
    

p2 <- ggplot(def_1v1_data_ind, aes(x = t_def_one_on_ones, y = loss_ratio, colour = playing_for)) + 
    geom_point() +
    scale_colour_manual(values = team_colours) +
    geom_text_repel(aes(label = name),size = 2.5) +
    facet_grid(~season) +
    labs(title = 'Essendon Defensive 1v1 Player Averages (2021-2022)', caption = '@crow_data_sci') +
    xlab('Average Defensive 1v1s per game')+
    ylab('Percentage Lost')+
    theme(
        legend.position = 'none'
    )
p1
p2

ggsave(p1, filename = 'exploration_stuff/afl_avg.png', dpi = 600, type = 'cairo',
       width = 16, height = 9, units = 'cm')
ggsave(p2, filename = 'exploration_stuff/ess_ind_avg.png', dpi = 600, type = 'cairo',
       width = 16, height = 9, units = 'cm')

season_team_losses <- fryzigg_data %>% 
    select(season, playing_for, contest_def_losses) %>% 
    drop_na() %>% 
    group_by(season, playing_for) %>% 
    summarise(
        t_def_1v1_losses = sum(contest_def_losses)
    ) %>% filter(season %in% 2020:2022)

ggplot(season_team_losses, 
       aes(y = reorder(playing_for, t_def_1v1_losses), x = t_def_1v1_losses, fill = playing_for)) +
    geom_bar(stat = 'identity')+
    scale_fill_manual(values = team_colours) +
    geom_text(aes(label = t_def_1v1_losses),nudge_x = 5) +
    xlab('Total Defensive Losses')+
    ylab('')+
    theme(
        legend.position = 'none'
    ) +
    facet_wrap(~season)



#### offensive 1v1s ####
season_choice = 2021

off_1v1_data <- fryzigg_data %>% 
    select(date, season, round, playing_for, contest_off_one_on_ones, contest_off_wins, height_cm) %>% 
    drop_na() %>%
    filter(season == season_choice, round %in% c(1:25)) %>% 
    group_by(season, playing_for) %>% 
    summarise(
        n = length(unique(round)),
        t_off_one_on_ones = sum(contest_off_one_on_ones)/n,
        t_off_one_on_ones_wins = sum(contest_off_wins)/n,
        win_ratio = round(t_off_one_on_ones_wins/t_off_one_on_ones*100,2),
        .groups = 'drop'
    )

p1 <- ggplot(off_1v1_data, aes(x = t_off_one_on_ones, y = win_ratio, colour = playing_for)) + 
    geom_point() +
    scale_colour_manual(values = team_colours) +
    geom_text_repel(aes(label = playing_for)) +
    ggtitle(label = paste0(season_choice, ' Offensive 1v1 Team Averages'), subtitle = '@crow_data_sci') +
    xlab('Average offensive 1v1s per game')+
    ylab('Win Percentage')+
    theme(
        legend.position = 'none'
    )

off_1v1_data_ind <- fryzigg_data %>% 
    select(date, season, round, playing_for, id, first_name, surname, contest_off_one_on_ones, contest_off_wins, height_cm) %>% 
    drop_na() %>% 
    filter(height_cm <= 185) %>%
    filter(season == season_choice, round %in% c(1:25)) %>% 
    group_by(season, playing_for, id, first_name, surname) %>% 
    summarise(
        n = length(unique(round)),
        t_off_one_on_ones = sum(contest_off_one_on_ones)/n,
        t_off_one_on_ones_wins = sum(contest_off_wins)/n,
        win_ratio = round(t_off_one_on_ones_wins/t_off_one_on_ones*100,2),
        .groups = 'drop'
    ) %>% 
    filter(n >= 5) %>% 
    arrange(desc(t_off_one_on_ones)) %>% 
    slice(1:25) %>% 
    rowwise() %>% 
    mutate(
        name = paste(strsplit(first_name,'')[[1]][1], surname)
    )


p2 <- ggplot(off_1v1_data_ind, aes(x = t_off_one_on_ones, y = win_ratio, colour = playing_for)) + 
    geom_point() +
    scale_colour_manual(values = team_colours) +
    geom_text_repel(aes(label = name)) +
    ggtitle(label = paste0(season_choice, ' Offensive 1v1 Player Averages'), subtitle = "Top 25 avg off 1v1s, 5+ games") +
    xlab('Average offensive 1v1s per game') +
    ylab('Win Percentage') +
    theme(
        legend.position = 'none'
    )

p1+p2
