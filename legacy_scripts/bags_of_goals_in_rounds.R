
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% 
    filter(goals >= 5) %>% 
    group_by(season, round) %>% 
    summarise(count = n(), j_nos = paste(jumper_no, collapse = ', ')) %>% 
    filter(
        round == count,
    ) %>% View()

afl7 <- afltables %>% filter(round == "7")

View(afl7)
View(afl6 %>% filter(season %in% c(1980, 1974, 1969)))

afltables %>% 
    filter(goals >= 5) %>% 
    group_by(season, round) %>% 
    summarise(count = n()) %>% 
    filter(round == count) %>% View()


