# devtools::install_github("jimmyday12/fitzRoy")

library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

afltables <- fitzRoy::get_afltables_stats()

sum_no_points <- afltables %>% select(date, id, first_name, surname, kicks, handballs, goals, behinds) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(
        disp  = kicks + handballs,
        score = goals*6 + behinds,
        game_total = row_number(),
    ) %>% 
    filter(min(date) > '1970-01-01') %>% 
    mutate(
        cumsum_disp  = cumsum(disp),
        cumsum_score = cumsum(score)
    ) %>% #stop here for 
    filter(cumsum_score == 0) %>% 
    filter(cumsum_disp == max(cumsum_disp))
View(sum_no_points)

rle_calc <- function(column) {
    t <- rle(column)
    return(max(t$lengths))
}

sum_no_points %>% arrange(id, date) %>% summarise(most_consecutive_games_without_a_point = rle_calc(cumsum_score)) %>% 
    arrange(-most_consecutive_games_without_a_point)


rle_calc(c(0,0,0,0,0,0,0))

test <- sum_no_points %>% filter(id == 12238) %>% ungroup()
rle_calc(test$cumsum_score)

View(sum_no_points)
