# library(readxl)
library(dplyr)
library(tidyr)

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
    'GWS Giants' = '#f15c22',
    'North Melbourne'= '#013b9f'
)

library(googlesheets4)

pos <- 'MID' #RUC, DEF, MID, FWD

sc_own <- googlesheets4::read_sheet('1KGQiJgPJ_8etFixBl4DzfAbkdoAF6eESDlEda9pCiYg',sheet = pos) %>% 
    mutate(ownership = 100 * ownership)

last_round <- max(sc_own$round)

sc_ruc_cross <- crossing(round = 1:last_round, sc_own %>% distinct(player_name, team_name, team_abbr))

sc_own_full <- left_join(sc_ruc_cross, sc_own, by = c('round', 'player_name','team_name','team_abbr')) %>% 
    mutate(ownership = if_else(is.na(ownership),0,ownership))

library(ggplot2)
library(gganimate)
library(hrbrthemes)
library(png)

sc_anim <- ggplot(sc_own_full, aes(y = ownership, x = round, group = player_name, colour = team_name)) +
    # annotation_raster(logo, xmin = 17, xmax = 23, ymin = -13, ymax = -19, interpolate = T) +
    # annotate('text', x = 18, y = 16.5, label = "bold(\"Useless AFL Stats\")", parse = T) +
    geom_line() + #aes(size = width)
    scale_size_manual(values = c("TRUE" = 2, "FALSE" = 0.5)) +
    geom_point(size = 1.5) +
    geom_text(aes(x = last_round + 0.5, label = if_else(ownership>20, player_name,'')), hjust = 0) +
    geom_segment(aes(xend = last_round, yend = ownership), linetype = 2) +
    scale_y_continuous(breaks = seq(0,100,10), minor_breaks = NULL, expand = c(0.05,0.02)) +
    scale_x_continuous(breaks = seq(1,last_round,1), minor_breaks = NULL, expand = c(0,0)) +
    theme_minimal() +
    xlab("") + ylab("") +
    ggtitle(paste("2020 SuperCoach",pos,"% ownership")) + #subtitle = "Useless AFL Stats"
    scale_color_manual(values = team_colours) +
    theme(
        legend.position = 'none',
        plot.margin = margin(5.5, 125, 5.5, 5.5),
        # panel.background = element_rect(colour = '#F0F0F0'),
        # plot.background = element_rect(fill = '#eeeeee')
    ) +
    transition_reveal(round) +
    coord_cartesian(clip = 'off')

# lad_anim
animate(sc_anim, nframes = 240, fps = 20, end_pause = 40, height = 10, width = 15, units = 'cm', res = 110)
anim_save(paste0('2020_',pos,'_ownership.gif'))

