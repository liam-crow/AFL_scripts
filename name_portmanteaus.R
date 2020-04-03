# devtools::install_github("jimmyday12/fitzRoy")

library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

afltables_all <- fitzRoy::get_afltables_stats()

names(afltables_all) <- to_snake_case(names(afltables))

afl_id <- afltables_all %>% group_by(id, first_name, surname) %>% 
  summarise(date = max(date), teams = paste(unique(playing_for), collapse = ' ')) %>% ungroup()

# portmanteau


afl_id %>% filter(first_name == surname)

n <- 1
afl_id %>% mutate(
  first_lastn = to_snake_case(str_sub(first_name, -n)),
  last_lastn  = to_snake_case(str_sub(surname, 1, n))
) %>% filter(
  first_lastn == last_lastn
) %>% arrange(desc(date))
