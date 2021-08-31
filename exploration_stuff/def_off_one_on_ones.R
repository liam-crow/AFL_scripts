library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
source("fryzigg_data.R")

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

season_choice = 2017

def_1v1_data <- fryzigg_data %>% 
    select(date, season, round = match_round, player_team, contest_def_one_on_ones, contest_def_losses, player_height_cm) %>% 
    drop_na() %>%
    filter(season == season_choice, round %in% c(1:25)) %>% 
    group_by(season, player_team) %>% 
    summarise(
        n = length(unique(round)),
        t_def_one_on_ones = sum(contest_def_one_on_ones)/n,
        t_def_one_on_ones_losses = sum(contest_def_losses)/n,
        loss_ratio = round(t_def_one_on_ones_losses/t_def_one_on_ones*100,2),
        .groups = 'drop'
    )

p1 <- ggplot(def_1v1_data, aes(x = t_def_one_on_ones, y = loss_ratio, colour = player_team)) + 
    geom_point() +
    scale_colour_manual(values = team_colours) +
    geom_text_repel(aes(label = player_team)) +
    ggtitle(label = paste0(season_choice, ' Defensive 1v1 Team Averages'), subtitle = '@crow_data_sci') +
    xlab('Average defensive 1v1s per game')+
    ylab('Loss Percentage')+
    theme(
        legend.position = 'none'
    )

def_1v1_data_ind <- fryzigg_data %>% 
    select(date, season, round = match_round, player_team, player_id, player_first_name, player_last_name, contest_def_one_on_ones, contest_def_losses, player_height_cm) %>% 
    drop_na() %>% 
    # filter(player_height_cm <= 185) %>% 
    filter(season == season_choice, round %in% c(1:25)) %>% 
    group_by(season, player_team, player_id, player_first_name, player_last_name) %>% 
    summarise(
        n = length(unique(round)),
        t_def_one_on_ones = sum(contest_def_one_on_ones)/n,
        t_def_one_on_ones_losses = sum(contest_def_losses)/n,
        loss_ratio = round(t_def_one_on_ones_losses/t_def_one_on_ones*100,2),
        .groups = 'drop'
    ) %>% 
    filter(n >= 5) %>% 
    arrange(desc(t_def_one_on_ones)) %>% 
    slice(1:25) %>% 
    rowwise() %>% 
    mutate(
        name = paste(strsplit(player_first_name,'')[[1]][1], player_last_name)
    )
    

p2 <- ggplot(def_1v1_data_ind, aes(x = t_def_one_on_ones, y = loss_ratio, colour = player_team)) + 
    geom_point() +
    scale_colour_manual(values = team_colours) +
    geom_text_repel(aes(label = name)) +
    ggtitle(label = paste0(season_choice, ' Defensive 1v1 Player Averages'), subtitle = "Top 25 avg def 1v1s, 5+ games") +
    xlab('Average defensive 1v1s per game')+
    ylab('Loss Percentage')+
    theme(
        legend.position = 'none'
    )

p1+p2

season_team_losses <- fryzigg_data %>% 
    select(season, player_team, contest_def_losses) %>% 
    drop_na() %>% 
    group_by(season, player_team) %>% 
    summarise(
        t_def_1v1_losses = sum(contest_def_losses)
    ) %>% filter(season %in% 2020:2021)

ggplot(season_team_losses, 
       aes(y = reorder(player_team, t_def_1v1_losses), x = t_def_1v1_losses, fill = player_team)) +
    geom_bar(stat = 'identity')+
    scale_fill_manual(values = team_colours) +
    geom_text(aes(label = t_def_1v1_losses),nudge_x = 5) +
    xlab('Total Defensive Losses')+
    ylab('')+
    theme(
        legend.position = 'none'
    ) +
    facet_wrap(~season)

fryzigg_data %>% 
    select(date, season, round = match_round, player_team, match_home_team, match_away_team, player_id, player_first_name, player_last_name, contest_def_one_on_ones, contest_def_losses) %>% 
    drop_na() %>% View()


