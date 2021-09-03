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

chosen_season <- 2021
marks_data_type <- fryzigg_data %>% 
    select(date, season, round, playing_for, id, first_name, surname, marks, contested_marks, marks_on_lead) %>% 
    drop_na() %>% 
    filter(season == chosen_season, round %in% c(1:25)) %>% 
    group_by(season, playing_for, id, first_name, surname) %>% 
    summarise(
        n = length(unique(round)),
        t_m = sum(marks),
        t_cont_m = sum(contested_marks),
        t_lead_m = sum(marks_on_lead),
        cont_m_pct = round(t_cont_m/t_m*100,2),
        lead_m_pct = round(t_lead_m/t_m*100,2),
        total_pct = cont_m_pct + lead_m_pct,
        .groups = 'drop'
    ) %>% 
    mutate(dist_from_0 = sqrt(cont_m_pct^2+lead_m_pct^2)) %>% 
    filter(n>15) %>% 
    arrange(desc(t_lead_m)) %>% 
    # slice(1:50) %>%
    rowwise() %>% 
    mutate(
        name = paste(strsplit(first_name,'')[[1]][1], surname)
    )


p1 <- ggplot(marks_data_type, aes(x = cont_m_pct, y = lead_m_pct, colour = playing_for)) + 
    geom_point() +
    scale_colour_manual(values = team_colours) +
    geom_text_repel(aes(label = if_else(dist_from_0>30, name, ''))) +
    ggtitle(label = paste0(chosen_season, ' H/A Mark Types as a Percentage of Total Marks'), 
            subtitle = "@crow_data_sci, 15+games played, Data: @Fryzigg") +
    xlab('Contested Mark %')+
    ylab('Marks on Lead %')+
    theme(
        legend.position = 'none'
    )
p1


fryzigg_data %>% 
    select(date, season, round, playing_for, id, first_name, surname, marks, contested_marks, marks_on_lead) %>%
    drop_na() %>% 
    mutate(
        t_marks = contested_marks + marks_on_lead,
        ratio = t_marks/marks
    ) %>% View()
