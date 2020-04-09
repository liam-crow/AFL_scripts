# devtools::install_github("jimmyday12/fitzRoy")
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- get_afltables_stats()
names(afltables) <- names(afltables) %>% snakecase::to_snake_case()

# data_disp <- 
afltables %>% 
    filter(date > "1921-01-01") %>% 
    group_by(home_team, month(date), day(date)) %>% 
    summarise(
        # s_goals = sum(goals),
        # s_kicks = sum(kicks, na.rm = T),
        s_disp = sum(handballs, na.rm = T) + sum(kicks, na.rm = T),
        # s_behinds = sum(behinds, na.rm = T),
        # s_tackles = sum(tackles, na.rm = T),
        # s_hscore = sum(home_score, na.rm = T),
        # s_ascore = sum(away_score, na.rm = T),
        # s_ho = sum(hit_outs, na.rm = T),
        # s_att = sum(attendance),
        # a_att = mean(attendance)
    ) %>% 
    filter(between(a_att, 19990, 20010))
    View()

write.csv(data_disp, 'data_disp.csv')
