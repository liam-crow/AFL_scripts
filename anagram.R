
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afltables %>% select(id, first_name, surname) %>% distinct() %>% rowwise() %>% 
    mutate(
        full_name = str_to_lower(paste0(first_name)),
        full_name = gsub('[ ]', '', full_name),
        alpha_full_name = paste(sort(unlist(str_split(full_name, ''))), collapse = '')
    ) %>% ungroup() %>% 
    group_by(alpha_full_name) %>% 
    summarise(
        count = n(),
        names = paste(first_name, collapse = ', ')
    ) %>% rowwise() %>% 
    mutate(
        no_unique = length(unique(str_to_lower(unlist(str_split(names, ', '))))),
        names_unique = paste(unique(str_to_lower(unlist(str_split(names, ', ')))), collapse = ', ')
    ) %>% View() 
    filter(no_unique > 1) %>% 
    select(names) %>% View()
    
