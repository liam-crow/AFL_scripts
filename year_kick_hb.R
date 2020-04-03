# devtools::install_github("jimmyday12/fitzRoy")

library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)

afltables <- fitzRoy::get_afltables_stats()

names(afltables) <- to_snake_case(names(afltables))

afl_id <- afltables_all %>% select(id, first_name, surname) %>% distinct()

res <- afltables %>% select(season, round, id, first_name, surname, kicks, handballs) %>% 
    filter(
        (season == 2001 & kicks == 20 & handballs == 1) |
        (season == 2002 & kicks == 20 & handballs == 2) |
        (season == 2003 & kicks == 20 & handballs == 3) |
        (season == 2004 & kicks == 20 & handballs == 4) |
        (season == 2005 & kicks == 20 & handballs == 5) |
        (season == 2006 & kicks == 20 & handballs == 6) |
        (season == 2007 & kicks == 20 & handballs == 7) |
        (season == 2008 & kicks == 20 & handballs == 8) |
        (season == 2009 & kicks == 20 & handballs == 9) |
        (season == 2010 & kicks == 20 & handballs == 10) |
        (season == 2011 & kicks == 20 & handballs == 11) |
        (season == 2012 & kicks == 20 & handballs == 12) |
        (season == 2013 & kicks == 20 & handballs == 13) |
        (season == 2014 & kicks == 20 & handballs == 14) |
        (season == 2015 & kicks == 20 & handballs == 15) |
        (season == 2016 & kicks == 20 & handballs == 16) |
        (season == 2017 & kicks == 20 & handballs == 17) |
        (season == 2018 & kicks == 20 & handballs == 18) |
        (season == 2019 & kicks == 20 & handballs == 19)
    ) %>% arrange(season)

write.csv(res, 'year_kick_hb.csv')
