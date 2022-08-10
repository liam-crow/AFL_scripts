
library(dplyr)
devtools::source_url("https://raw.githubusercontent.com/DataByJosh/AFL-Data/main/AFLM_Match_Chains/Scraper.R")

chain_round <- get_match_chains(2021)
names(chain_round) <- snakecase::to_snake_case(names(chain_round))

chain_clean <- chain_round %>% 
    rename(
        playing_for = team_team_name,
        home_team = home_team_team_name,
        away_team = away_team_team_name,
        home_team_score = home_team_score_total_score,
        away_team_score = away_team_score_total_score,
        home_team_direction = home_team_direction_qtr_1
    ) %>% 
    mutate(
        h_a = if_else(playing_for == home_team, 'H', 'A')
    )

library(ggplot2)
library(ggforce)

ggplot(chain_clean %>% filter(shot_at_goal == T), aes(x,y,colour = playing_for)) +
    geom_ellipse(aes(x0 = 0, y0 = 0, a = 75, b = 65, angle = 0), fill = '#398c0a') +
    geom_point() +
    geom_segment(aes(x = 50, y = 60, xend = 75, yend = 60), arrow = arrow(), inherit.aes = F) +
    ggtitle('Every shot at goal in Rd 1 2021') +
    coord_fixed() + ylab('') + xlab('')

