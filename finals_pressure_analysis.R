library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
source("load_afltables.R")
source("fryzigg_data.R")

prep_data <- fryzigg_data %>% 
    select(
        season, date, round = match_round,
        player_team,
        id = player_id, first_name = player_first_name, surname = player_last_name, 
        disposals, disposal_efficiency_percentage, goals, behinds, score_launches, 
        supercoach_score, fantasy_points, rating_points
    ) %>% 
    drop_na() %>% 
    mutate(
        final_yn = if_else(round %in% 1:25, 'h_a', 'final'),
        accuracy = goals/score_launches*100
    )

player_data <- prep_data %>% 
    filter(first_name == 'Patrick', surname == 'Dangerfield')

player_data <- prep_data %>% 
    filter(player_team == 'Geelong')

library(ggplot2)

ggplot(
        player_data %>% filter(disposals >= 5),
        aes(x = date, y = rating_points, colour = final_yn)
    ) +
    geom_point() +
    geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k=5))
    
ggplot(player_data %>% filter(disposals >= 5)) +
    geom_boxplot(aes(x = final_yn, y = rating_points, colour = final_yn))
