
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- fitzRoy::get_afltables_stats(start_date = "1900-01-01")
names(afltables) <- snakecase::to_snake_case(names(afltables))

afltables %>% 
    group_by(season) %>% 
    summarise(
        sum_goals = sum(goals),
        sum_behinds = sum(behinds)
    ) %>% 
    mutate(
        sum_goals = if_else(is.na(sum_goals), 0, sum_goals),
        sum_behinds = if_else(is.na(sum_behinds), 0, sum_behinds),
        acc = sum_goals/(sum_goals + sum_behinds)
    ) %>% 
    filter(
        sum_goals > 40,
        season > 1965
    ) %>% arrange(-acc)

# accuracy by season
afltables %>% 
    group_by(season, id, first_name, surname) %>% 
    summarise(
        sum_goals = sum(goals),
        sum_behinds = sum(behinds)
    ) %>% 
    mutate(
        sum_goals = if_else(is.na(sum_goals), 0, sum_goals),
        sum_behinds = if_else(is.na(sum_behinds), 0, sum_behinds),
        acc = sum_goals/(sum_goals + sum_behinds)
    ) %>% 
    filter(
        sum_goals > 40,
        season > 1965
    ) %>% arrange(-acc) %>% View()

# accuracy most goals kicked per season
coleman_inacc <- afltables %>% group_by(season, id, first_name, surname) %>% distinct() %>%
    filter(!round %in% c('EF','PF','SF','GF','QF')) %>%
    summarise(
        sum_goals = sum(goals),
        sum_behinds = sum(behinds)
    ) %>% 
    mutate(
        sum_goals = if_else(is.na(sum_goals), 0, sum_goals),
        sum_behinds = if_else(is.na(sum_behinds), 0, sum_behinds),
        acc = sum_goals/(sum_goals + sum_behinds)
    ) %>% 
    filter(
        sum_goals > 40,
        season > 1965
    ) %>% 
    group_by(season) %>% 
    filter(sum_goals == max(sum_goals))

mean(coleman_inacc$acc)
View(coleman_inacc)

# accuracy by season and round
afltables %>% select(season, round, home_team, hq_4_g, hq_4_b, away_team, aq_4_g, aq_4_b) %>% 
    distinct() %>% 
    group_by(season, round) %>% 
    summarise(
        sum_goals = sum(hq_4_g) + sum(aq_4_g),
        sum_behinds = sum(hq_4_b) + sum(aq_4_b)
    ) %>% ungroup() %>%  
    mutate(
        # sum_goals = if_else(is.na(sum_goals), 0, sum_goals),
        # sum_behinds = if_else(is.na(sum_behinds), 0, sum_behinds),
        acc = sum_goals/(sum_goals + sum_behinds)
    ) %>% 
    filter(
        # sum_goals > 40,
        season > 1965
    ) %>% 
    # group_by(season) %>% 
    arrange(acc) %>% View()

# accuracy by season and round
afltables %>% select(season, round, home_team, hq_4_g, hq_4_b, away_team, aq_4_g, aq_4_b) %>% 
    distinct() %>% 
    group_by(season, round) %>% 
    summarise(
        sum_goals = sum(hq_4_g) + sum(aq_4_g),
        sum_behinds = sum(hq_4_b) + sum(aq_4_b)
    ) %>% ungroup() %>%  
    mutate(
        # sum_goals = if_else(is.na(sum_goals), 0, sum_goals),
        # sum_behinds = if_else(is.na(sum_behinds), 0, sum_behinds),
        acc = sum_goals/(sum_goals + sum_behinds)
    ) %>% 
    filter(
        # sum_goals > 40,
        season > 1965
    ) %>% 
    # group_by(season) %>% 
    arrange(acc) %>% View()
