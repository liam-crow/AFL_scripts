
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

View(afltables)

afltables %>% 
    select(id, first_name, surname) %>% distinct() %>% 
    filter(
        grepl('^V', first_name),
        grepl('^W', surname)
    )
