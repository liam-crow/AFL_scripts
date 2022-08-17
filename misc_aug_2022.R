
library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
library(stringr)
library(ggplot2)
source("load_afltables.R")
source("load_fryzigg.R")

afltables %>% 
    select(season, round, playing_for, id, first_name, surname, games_played) %>% 
    filter(games_played >= 250) %>% 
    group_by(season, round, playing_for) %>% 
    summarise(
        n = n(),
        comb = paste0(surname, ' (',games_played,')', collapse = ', ')
    ) %>% View()


afltables %>%
    select(date, playing_for, opp, venue) %>%
    distinct() %>%
    group_by(playing_for, opp) %>%
    mutate(
        venue_1 = lag(venue,1),
        venue_2 = lag(venue,2),
        venue_3 = lag(venue,3),
        venue_4 = lag(venue,4),
        venue_5 = lag(venue,5),
        venue_6 = lag(venue,6),
        # venue_7 = lag(venue,7),
        # venue_8 = lag(venue,8),
    ) %>%
    rowwise() %>%
    mutate(
        n_dist = n_distinct(list(venue,venue_1,venue_2,venue_3,venue_4,venue_5,venue_6))
    ) %>% View()

fn <- afltables %>%
    select(id, first_name) %>%
    distinct() %>%
    group_by(first_name) %>% count() %>% ungroup() %>%
    arrange(-n) %>%
    mutate(rank = rank(-n, ties.method = 'min'))

sn <- afltables %>%
    select(id, surname) %>%
    distinct() %>%
    group_by(surname) %>% count() %>% ungroup() %>%
    arrange(-n) %>%
    mutate(rank = rank(-n, ties.method = 'min'))

all <- afltables %>% expand(first_name, surname) %>%
    inner_join(fn, by = 'first_name') %>%
    inner_join(sn, by = 'surname')


non_players <- all %>% anti_join(afltables %>% select(first_name, surname)) %>%
    mutate(
        t_rank = rank.x + rank.y
    ) %>%
    arrange(-t_rank) %>%
    select(first_name, surname, t_rank)


write.csv(non_players, "fake_afl_players/fake_afl_players.csv", row.names = F)

ind_games <- afltables %>% 
    filter(id == 4088) %>% 
    select(date, home_team, away_team) 

afltables %>% 
    filter(playing_for != 'Fremantle') %>% 
    select(date, home_team, away_team, id, first_name, surname) %>% 
    inner_join(ind_games, by = c('date','home_team','away_team')) %>% 
    group_by(id, first_name, surname) %>% count() %>% View()

afltables %>% 
    select(date, round, playing_for, id, first_name, surname) %>% 
    mutate(si = str_trunc(surname, 1, side = 'right', ellipsis = '')) %>%
    group_by(date, round, playing_for, si) %>% 
    summarise(
        n = n(),
        surnames = paste0(surname, collapse = ', ')
    ) %>% View()

afltables %>% 
    filter(season > 2000) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        avg_disp = mean(disposals),
        avg_goals= mean(goals)
    ) %>% View()

robbie <- afltables %>% 
    filter(id == 11578) %>% 
    select(date, disposals, goals)

p1 <- ggplot(robbie, aes(x = date, y = disposals)) +
    geom_point() +
    geom_smooth() +
    ggdark::dark_mode() +
    labs(
        title = 'Shades of Robbie Gray',
        subtitle = 'Average Disposals and Goals over his career'
    )


p2 <- ggplot(robbie, aes(x = date, y = goals)) +
    geom_point() +
    geom_smooth() +
    ggdark::dark_mode()

p1/p2

afltables %>% 
    select(date, round, home_team, away_team, playing_for, id, first_name, surname) %>% 
    mutate(si = str_trunc(surname, 1, side = 'right', ellipsis = '')) %>% 
    group_by(date, round, home_team, away_team, playing_for) %>% 
    mutate(
        n_players = n(),
        nunique = n_distinct(si)
    ) %>% 
    group_by(date, round, home_team, away_team, playing_for, si) %>% 
    mutate(n = n_di) %>% View()

afltables %>% 
    filter(season == 2022) %>% 
    select(playing_for, id, first_name, surname) %>% 
    distinct() %>% 
    mutate(f_n = nchar(first_name),
           s_n = nchar(surname),
           t = f_n + s_n) %>% 
    group_by(playing_for) %>% 
    filter(t == min(t)) %>% View()

afltables %>% 
    select(date, id, first_name, surname, playing_for, opp, w_l, playing_for_score, opp_score) %>% 
    filter(surname == 'Cripps', opp == 'Sydney') %>%
    group_by(id, first_name, surname) %>% 
    summarise(
        tw = sum(w_l == 'W'),
        td = sum(w_l == 'D'),
        tl = sum(w_l == 'L'),
        .groups = 'drop'
    ) %>% gt::gt()

afltables %>% 
    select(date,round, id, first_name, surname, stat = disposals) %>% 
    drop_na() %>% 
    group_by(id, first_name, surname) %>% 
    mutate(
        stat1 = lag(stat,1),
        stat2 = lag(stat,2),
        stat3 = lag(stat,3),
        stat4 = lag(stat,4),
        total_stat = stat+stat1+stat2+stat3+stat4
    ) %>% View()

afltables %>% 
    select(season, round,id, first_name, surname,w_l) %>% 
    filter(w_l == 'W', round == 'GF') %>% 
    group_by(first_name) %>% 
    summarise(
        n = n(),
        surnames = paste0(surname, collapse = ', ')
    ) %>% View()

afltables %>% 
    select(season, date, playing_for, w_l) %>% distinct() %>% 
    group_by(playing_for) %>% 
    filter(season == min(season)) %>% 
    group_by(season, playing_for) %>% 
    summarise(
        tw = sum(w_l == 'W'),
        td = sum(w_l == 'D'),
        tl = sum(w_l == 'L'),
        .groups = 'drop'
    ) %>%
    mutate(w_l_ratio = tw/(tw+td+tl))

pcnt_data <- afltables %>% 
    filter(round %in% 1:25) %>% 
    select(season, date, playing_for, playing_for_score, opp_score) %>% distinct() %>% 
    group_by(playing_for) %>% 
    group_by(season, playing_for) %>% 
    summarise(
        tfor = sum(playing_for_score),
        tagainst = sum(opp_score),
        .groups = 'drop'
    ) %>%
    mutate(pnct = round(tfor/tagainst*100,2))

afltables %>% 
    select(season, playing_for, round, w_l) %>% 
    filter(round == 'GF', w_l == 'L') %>% 
    distinct() %>% 
    inner_join(pcnt_data, by = c("season", "playing_for")) %>% View()

#  Is it possible to see the % of players that have kicked a goal in their last game?

afltables %>% 
    select(id, first_name, surname, goals, games_played) %>% 
    group_by(id, first_name, surname) %>% 
    filter(games_played == max(games_played)) %>% 
    mutate(gtf = goals > 0) %>% 
    group_by(gtf) %>% count() %>% View()
# 18.8%   

# goals and behinds for day vs night?

afltables %>% 
    filter(season >= 1965) %>% 
    select(local_start_time, goals, behinds) %>%
    mutate(
        local_start_time = as.numeric(local_start_time),
        time_of_day = case_when(
            local_start_time <= 1600 ~ 'day',
            local_start_time <= 1800 ~ 'twilight',
            T ~ 'night',
        )
    ) %>% 
    group_by(time_of_day) %>% 
    summarise(
        tgoals = sum(goals),
        tbehinds = sum(behinds),
        acc = round(tgoals/(tgoals+tbehinds)*100,2)
    ) %>% View()


# Not sure if they have this data but who is the shortest 
# AFL player to ever get 10+ hitouts in a game?

fryzigg_data %>% 
    select(season, round,playing_for, id, first_name, surname, height_cm, hit_outs) %>% 
    drop_na() %>% 
    filter(hit_outs > 5) %>% View()

fryzigg_data %>% 
    select(season, round, id, first_name, surname, supercoach_score) %>% View()

# Which jumper number has kicked the most and least number 
# of goals in VFL/AFL history? And which jumper number has 
# played the most and least number of games?

afltables %>% 
    group_by(jumper_no) %>% 
    summarise(
        sg = sum(goals),
        n = n()
    ) %>% View()

afltables %>% 
    filter(season > 2006) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        n = n(),
        avg_pcnt_og = mean(time_on_ground)
    ) %>% View()
    
