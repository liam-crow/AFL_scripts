library(readxl)
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
    'Greater Western Sydney' = '#f15c22',
    'North Melbourne'= '#013b9f'
)

year <- '1994'
ladder <- readxl::read_excel("ladders.xlsx", sheet = year)

names(ladder) <- snakecase::to_snake_case(names(ladder))

ladder_long <- ladder %>% 
    mutate(team = gsub('\\*','',team)) %>% 
    pivot_longer(
        -team,
        names_to = 'round',
        values_to = 'rank'
    ) %>% 
    mutate(round = as.integer(round))

last_round <- max(ladder_long$round)

# ladder_long %>% 
#     group_by(round) %>% 
#     summarise(
#         unique_chars = length(unique(rank))
#     ) %>% View()

library(ggplot2)
library(gganimate)
library(hrbrthemes)
library(png)
logo <- readPNG("MediumSquareLogo.png")

ladder_long <- ladder_long %>% 
    mutate(width = if_else(team == 'Carlton',T,F))

lad_anim <- ggplot(ladder_long, aes(y = rank, x = round, group = team, colour = team)) +
    # annotation_raster(logo, xmin = 17, xmax = 23, ymin = -13, ymax = -19, interpolate = T) +
    # annotate('text', x = 18, y = 16.5, label = "bold(\"Useless AFL Stats\")", parse = T) +
    geom_line(aes(size = width)) +
    scale_size_manual(values = c("TRUE" = 2, "FALSE" = 0.5)) +
    geom_point(size = 1.5) +
    geom_text(aes(x = last_round + 0.5, label = team), hjust = 0) +
    geom_segment(aes(xend = last_round, yend = rank), linetype = 2) +
    geom_segment(aes(x = 1, y = 8.5, xend = last_round, yend = 8.5), colour = 'red', size = 1.1) +
    scale_y_reverse(breaks = seq(1,18,1), minor_breaks = NULL, expand = c(0.02,0.02)) +
    scale_x_continuous(breaks = seq(1,last_round,1), minor_breaks = NULL, expand = c(0,0)) +
    theme_minimal() +
    xlab("") + ylab("") +
    ggtitle(paste(year ,"AFL Ladder"), subtitle = "Useless AFL Stats") +
    scale_color_manual(values = team_colours) +
    theme(
        legend.position = 'none',
        plot.margin = margin(5.5, 125, 5.5, 5.5),
        # panel.background = element_rect(colour = '#F0F0F0'),
        # plot.background = element_rect(fill = '#eeeeee')
    ) +
    transition_reveal(round) +
    coord_cartesian(clip = 'off')

lad_anim
animate(lad_anim, nframes = 440, fps = 20, end_pause = 40, height = 10, width = 15, units = 'cm', res = 110)
anim_save(paste0('ladder_',year,'_UAS.gif'))
