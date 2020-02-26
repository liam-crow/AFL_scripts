# devtools::install_github("jimmyday12/fitzRoy")
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
    'Rich'= '#fed102',
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

afltables_clangers <- afltables_modern %>% group_by(round, playing_for) %>% 
    summarise(
        sum_k = sum(kicks),
        sum_h = sum(handballs),
        sum_c = sum(clangers)
    ) %>% mutate(sum_disp = sum_k + sum_h)

theme_set(theme_minimal())
p <-
    ggplot(
        afltables_clangers%>% filter(round < 2), 
        aes(x = sum_c, y = sum_disp, color = playing_for, label = playing_for)
    ) + 
    geom_point() +
    geom_text(aes(label = playing_for), hjust = -0.1, vjust = 0) +
    labs(x = "Clangers", y = "Disposals") + 
    theme(legend.position = "none") +
    scale_color_manual(values = team_colours)

p_a <- p + transition_time(round) + 
    labs(title = "Season 2019, Round {frame_time}") +
    enter_fade() + exit_fade() +
    ease_aes('sine-in-out')

animate(p_a, nframes = 200)

anim_save("Clangers_v_Disposals.gif")





