
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

goals_year <- afltables %>% 
    select(season, round, playing_for, id, first_name, surname, goals) %>% 
    group_by(season, goals) %>% count()

library(ggplot2)
library(ggpubr)
library(ggsci)
library(forcats)
library(viridis)
library(patchwork)

theme_set(
    theme_pubr() +
        theme(legend.position = "none")
)

p <- ggplot(goals_year %>% 
                filter(
                    between(season, 1910,2019),
                    goals > 8
                )
            ) + scale_fill_viridis(discrete = TRUE, direction = -1)

p1 <- p + geom_bar(
    aes(fill = forcats::fct_rev(as.factor(goals)), y = n, x = season), 
    position = 'stack', stat = "identity", width = 1
)

p2 <- p + geom_bar(
    aes(fill = forcats::fct_rev(as.factor(goals)), y = n, x = season), 
    position = position_fill(), stat = "identity", width = 1
)

p1 / p2
