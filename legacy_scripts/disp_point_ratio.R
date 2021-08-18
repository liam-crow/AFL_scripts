# devtools::install_github("jimmyday12/fitzRoy")

library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

afltables <- fitzRoy::get_afltables_stats()

disp_point_ratio <- afltables %>% select(date, id, first_name, surname, kicks, handballs, goals, behinds) %>% 
    mutate(
        disp  = kicks + handballs,
        score = goals*6 + behinds
    ) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        debut = min(date),
        sum_disp  = sum(disp),
        sum_score = sum(score)
    ) %>% 
    filter(debut > "1970-01-01") %>%
    mutate(ratio = sum_score/sum_disp) %>% 
    arrange(-ratio)

View(disp_point_ratio)
write.csv(disp_point_ratio, "disp_point_ratio.csv")
