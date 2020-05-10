
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

modern_afl_b_op <- afltables %>% 
    filter(date > '1999-01-01', date < "2020-01-01") %>% 
    select(season, id, first_name, surname, bounces, one_percenters) %>% 
    group_by(season) %>% 
    summarise(
        Bounces = sum(bounces),
        `One Percenters` = sum(one_percenters)
    ) %>% 
    pivot_longer(
        -season,
        names_to  = 'Stat',
        values_to = 'Sum'
    )


library(ggplot2)
library(patchwork)

ggplot(modern_afl_b_op, aes(x = season, y = Sum, colour = Stat)) + geom_line(show.legend = T) + 
    theme(legend.position = 'bottom', legend.title = element_blank()) +
    xlim(c(1999,2020)) + ylim(c(0, 22500)) +
    xlab('Season') +
    ylab('') +
    ggtitle('Bounces and One Percenters over Time')



sum(modern_afl$bounces)
sum(modern_afl$one_percenters)


