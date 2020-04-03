# devtools::install_github("jimmyday12/fitzRoy")
# install.packages('ggrepel')
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- get_afltables_stats()
names(afltables) <- names(afltables) %>% snakecase::to_snake_case()

team_colours <- c(
    'Carl'= '#0e1e2d',
    'Geel'= '#1c3c63',
    'Adel'= '#002b5c',
    'Syd' = '#ed171f',
    'StK' = '#ed0f05',
    'Ess' = '#cc2031',
    'Rich'= '#e0d016',
    'Melb'= '#0f1131',
    'Haw' = '#4d2004',
    'BL'  = '#a30046',
    'GCS' = '#d93e39',
    'Freo'= '#2a1a54',
    'Coll'= '#000000',
    'PA'  = '#01b5b6',
    'WB'  = '#014896',
    'WCE' = '#062ee2',
    'GWS' = '#f15c22',
    'NthM'= '#013b9f'
)

afltables_modern <- afltables %>% 
    filter(
        date > "2019-01-01",
        !(round %in% c('EF','QF','SF','PF','GF'))
    ) %>% 
    mutate(
        round = as.integer(round),
        playing_for = case_when(
            playing_for == 'Carlton' ~ 'Carl',
            playing_for == 'Geelong' ~ 'Geel',
            playing_for == 'Adelaide' ~ 'Adel',
            playing_for == 'Sydney' ~ 'Syd',
            playing_for == 'St Kilda' ~ 'StK',
            playing_for == 'Essendon' ~ 'Ess',
            playing_for == 'Richmond' ~ 'Rich',
            playing_for == 'Melbourne' ~ 'Melb',
            playing_for == 'Hawthorn' ~ 'Haw',
            playing_for == 'Brisbane Lions' ~ 'BL',
            playing_for == 'Gold Coast' ~ 'GCS',
            playing_for == 'Fremantle' ~ 'Freo',
            playing_for == 'Collingwood' ~ 'Coll',
            playing_for == 'Port Adelaide' ~ 'PA',
            playing_for == 'Western Bulldogs' ~ 'WB',
            playing_for == 'West Coast' ~ 'WCE',
            playing_for == 'Greater Western Sydney' ~ 'GWS',
            playing_for == 'North Melbourne' ~ 'NthM'
        )
    )

afltables_k_hb <- afltables_modern %>% 
    select(round, playing_for, id, first_name, surname, kicks, handballs) %>% 
    mutate(
        label_tf = if_else(sqrt(kicks^2 + handballs^2) >= 26, T, F),
        first_name_initial = substring(first_name, 1, 1)
    ) %>% 
    unite(
        names_short, first_name_initial, surname, sep = ' '
    )

quantile(afltables_k_hb$kicks, probs = c(.9)) # 16 =< kicks
quantile(afltables_k_hb$handballs, probs = c(.9)) # 13 =< handballs
library(ggplot2)
library(gganimate)
library(ggrepel)

theme_set(theme_light())
p <-
    ggplot(
        afltables_k_hb,# %>% filter(round == 4), 
        aes(x = kicks, y = handballs, color = playing_for)
    ) + 
    geom_point() +
    geom_text_repel(aes(label = if_else(label_tf == T, names_short, NULL), fontface = 2), hjust = -0.2, vjust = 0) +
    labs(x = "Kicks", y = "Handballs") + 
    theme(legend.position = "none") +
    scale_color_manual(values = team_colours)
p

p_a <- p + transition_time(round) + 
    labs(title = "Kicks vs Handballs per Player per Round
Season 2019, Round {frame_time}, Useless AFL Stats") +
    enter_fade() + exit_fade() +
    ease_aes('quartic-in-out')

animate(p_a, nframes = 700, fps = 16, end_pause = 40)

anim_save("Kicks_v_Handballs.gif")





