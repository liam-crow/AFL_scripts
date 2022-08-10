
library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
library(stringr)
source("load_afltables.R")
library(ggplot2)
library(ggdark)


gk_data <- afltables %>% 
    select(season, date, playing_for_short, id, first_name, surname, goals) %>% 
    filter(goals != 0) %>% 
    mutate(id = as.character(id)) %>% 
    group_by(id, first_name, surname) %>% 
    mutate(
        cum_goals = cumsum(goals),
        max_goals = max(cum_goals)
    ) %>% 
    group_by(season, id, first_name, surname) %>% 
    filter(date == min(date) | date == max(date))

player_colours <- 
    c(
        '2245' = "#0c7ecf", #geelong doug wade
        '567'  = "#0c7ecf", # Gary Ablett Snr Geel
        '990'  = "#ed171f", #stk swans Tony Lockett
        '4065' = "#ed171f", # lance franklin hawks swans
        '1628' = "#ffffff", #Gordon Coverntry Coll
        '632'  = "#fabf16" #Jason Dunstall Hawks
    )

player_alpha <- 
    c(
        '2245' = 1, #geelong doug wade
        '990'  = 1, #stk swans Tony Lockett
        '4065' = 1, # lance franklin hawks swans
        '567'  = 1, # Gary Ablett Snr Geel
        '1628' = 1, #Gordon Coverntry Coll
        '632'  = 1 #Jason Dunstall Hawks
    )

plot <- 
    ggplot(
        gk_data %>% filter(),
        aes(x = date, y = cum_goals, group = id, colour = id, alpha = id)
    ) +
    geom_line(size = 0.3) +
    labs(
        x = NULL, y = NULL
    ) +
    dark_theme_classic() +
    scale_colour_manual(values = player_colours, na.value = "grey") +
    scale_alpha_manual(values = player_alpha, na.value = 0.1) +
    theme(
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = 'none'
    )

ggsave(plot, filename = 'exploration_stuff/buddy_1k.png', 
       dpi = 300, type = 'cairo',
       width = 16, height = 9, units = 'cm')

afltables