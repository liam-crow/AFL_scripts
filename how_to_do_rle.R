
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

# 1	30 Andrew McLeod
# 2	1114 Jordan	McMahon

library(purrr)
library(tidyr)

rle_tbl <- function(col) {
    rle_res <- rle(col)
    data_frame(
        len = rle_res$lengths,
        val = rle_res$values
    )
}

consecutive_results <- afltables %>% 
    filter(season > 1964) %>% 
    select(season, date, id, first_name, surname, disposals, goals) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(
        diff_from_prev_disp = disposals - lag(disposals),
        diff_disp_tf = if_else(between(diff_from_prev_disp, -1, 1), T, F)
    ) %>%
    nest(-id, -first_name, -surname) %>% 
    mutate(
        rle_res = map(data, ~rle_tbl(col = .x$diff_disp_tf))
    ) %>% 
    unnest(rle_res)

consecutive_results %>% View()

consecutive_results_g <- afltables %>% 
    filter(season > 1964) %>% 
    select(season, date, id, first_name, surname, disposals, goals) %>% 
    group_by(id, first_name, surname) %>% 
    arrange(date) %>% 
    mutate(
        diff_from_prev_goals = goals - lag(goals),
        diff_disp_tf = if_else(between(diff_from_prev_goals, -1, 1), T, F),
        diff_disp_tf = if_else(goals == 0, F, diff_disp_tf)
    ) %>%
    nest(-id, -first_name, -surname) %>% 
    mutate(
        rle_res = map(data, ~rle_tbl(col = .x$diff_disp_tf))
    ) %>% 
    unnest(rle_res)

consecutive_results_g %>% View()
