# devtools::install_github("jimmyday12/fitzRoy")

library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

afltables <- fitzRoy::get_afltables_stats()
aflwtables <- fitzRoy::get_aflw_match_data()
aflwtables_detailed <- fitzRoy::get_aflw_detailed_data(matchids = aflwtables$Match.Id[1:110]) #This number changes as the season pogresses



names(afltables_all) <- to_snake_case(names(afltables))

afl_id <- afltables %>% select(id, first_name, surname) %>% distinct()


library(stringi)
is.palindrome <- function(x) stri_reverse(x) == x

afl_id %>% mutate(comb = sub('^.|.$', '', paste0(first_name, surname))) #%>% 
    filter(is.palindrome(str_to_lower(comb)) == T) #%>% 
    group_by(first_name) %>% count()
