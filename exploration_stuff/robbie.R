
library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
library(stringr)
source("load_afltables.R")
library(ggplot2)
library(ggdark)


gk_data <- afltables %>% 
    select(season, date, playing_for, id, first_name, surname, goals) %>% 
    filter(goals != 0, playing_for == 'Port Adelaide') %>% 
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
        '831' = "#008080", #tredrea
        '11578'  = "#008080" #gray
    )

player_alpha <- 
    c(
        '831' = 1, #tredrea
        '11578'  = 1 #gray
    )

plot <- 
    ggplot(
        gk_data,
        aes(x = date, y = cum_goals, group = id, colour = id, alpha = id)
    ) +
    geom_line(size = 0.9) +
    labs(
        x = NULL, y = NULL
    ) +
    dark_theme_classic() +
    scale_colour_manual(values = player_colours, na.value = "grey") +
    scale_alpha_manual(values = player_alpha, na.value = 0.5) +
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