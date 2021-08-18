# devtools::install_github("jimmyday12/fitzRoy")

library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

afltables <- fitzRoy::get_afltables_stats()

names(afltables) <- to_snake_case(names(afltables))

afl_id <- afltables %>% select(id, first_name, surname) %>% distinct()

afl_id %>% mutate(
    tf = str_detect(str_to_lower(first_name), "^[^aeiou]*$",negate = F),
    chars = nchar(first_name)
) %>% filter(tf == T) %>% arrange(-chars)
