
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables

afltables_brownlow <- afltables %>% 
    filter(season > 2010)

p <- ggplot(afltables_brownlow)

p1 <- 
    p + geom_density(
        aes(
            x = disposals, 
            group = as.character(brownlow_votes), 
            fill = as.character(brownlow_votes)), 
            alpha = 0.5, adjust = 1.4
        ) +
    ggtitle("Disposals by Brownlow Vote") + 
    theme(legend.position = "left") + 
    labs(fill = 'Votes') + xlab('Disposals')

p2 <- 
    p + geom_density(
        aes(
            x = kicks, 
            group = as.character(brownlow_votes), 
            fill = as.character(brownlow_votes)), 
            alpha = 0.5, adjust = 1.4
        ) +
    ggtitle("Kicks by Brownlow Vote") + 
    theme(legend.position = "none") + 
    labs(fill = 'Votes') + xlab('Kicks')

p3 <- 
    p + geom_density(
        aes(
            x = handballs, 
            group = as.character(brownlow_votes), 
            fill = as.character(brownlow_votes)), 
            alpha = 0.5, adjust = 1.6
        ) +
    ggtitle("Handballs by Brownlow Vote") + 
    theme(legend.position = "none") + 
    labs(fill = 'Votes') + xlab('Handballs')

p4 <- 
    p + geom_density(
        aes(
            x = tackles, 
            group = as.character(brownlow_votes), 
            fill = as.character(brownlow_votes)), 
            alpha = 0.5, adjust = 2.5
        ) +
    ggtitle("Tackles by Brownlow Vote") + 
    theme(legend.position = "none") + 
    labs(fill = 'Votes') + xlab('Goals')

(p1+p2)/(p3+p4)
