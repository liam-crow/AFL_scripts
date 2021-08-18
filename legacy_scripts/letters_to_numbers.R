
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- fitzRoy::get_afltables_stats(start_date = "1900-01-01")
names(afltables) <- snakecase::to_snake_case(names(afltables))

library(mgsub)
library(gmp)
lett_to_num <- 
    afltables %>% select(id, first_name, surname) %>% 
    unite(name, first_name, surname, sep = '') %>% 
    mutate(
        name = tolower(name), 
        char = nchar(name), 
        name = gsub('[- ]','',name)
    ) %>% 
    distinct() %>% rowwise() %>% 
    mutate(
        conv = trimws(mgsub(string = name, pattern = letters, replacement = paste0(1:26, ' '))),
        prod = prod(as.numeric(unlist(strsplit(conv, ' ')))),
        add = sum(as.numeric(unlist(strsplit(conv, ' ')))),
        avg = mean(as.numeric(unlist(strsplit(conv, ' ')))),
        is_prime = all(as.logical(gmp::isprime(as.numeric(unlist(strsplit(conv, ' ')))))),
        add_is_prime = as.logical(is_prime(add))
    )
View(lett_to_num)
    
afltables %>% filter(date = '2010-05-05')
