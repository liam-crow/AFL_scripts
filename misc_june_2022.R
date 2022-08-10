
library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
library(stringr)
library(ggplot2)
source("load_afltables.R")
source("load_fryzigg.R")

gf_2000 <- afltables %>% 
    filter(round == 'GF', season >= 1900) %>% 
    select(season, playing_for, opp) %>% 
    distinct() %>% 
    group_by(playing_for,opp) %>% 
    summarise(season = paste0(season, collapse = ', '))

afltables %>% 
    filter(season >= 1900) %>% 
    select(season,round, home_team, away_team) %>% 
    distinct() %>% 
    inner_join(gf_2000, by = c('home_team'='playing_for','away_team'='opp'), suffix = c('','_gf')) %>% 
    group_by(season, round) %>% 
    mutate(n = n()) %>% 
    arrange(-n,season,round,season_gf) %>% View()

player_merge <- afltables %>% 
    select(season, round, home_team, away_team, id, first_name, surname, stat = bounces)

player_merge %>% 
    inner_join(
        player_merge, 
        by = c('season', 'round', 'home_team', 'away_team', 'surname' = 'first_name'),
        suffix = c('_1','_2')
    ) %>% 
    mutate(
        comb_stat = stat_1 + stat_2
    ) %>% 
    select(season, round, home_team, away_team, first_name, surname, surname_2, stat_1, stat_2, comb_stat) %>%
    arrange(-comb_stat) %>% 
    head(100) %>% 
    View()

afltables %>% 
    select(season, round, date,playing_for, id, first_name, surname, goals, behinds, w_l) %>% 
    arrange(desc(date)) %>% 
    filter(goals == 0, behinds == 5, w_l == 'W') %>% View()

afltables %>% 
    select(season, round, playing_for, jumper_no) %>% 
    filter(round %in% c(1,2), jumper_no > 0) %>% 
    distinct(season, jumper_no, playing_for) %>% 
    group_by(season, jumper_no) %>% 
    summarise(
        n = n()
    ) %>% View()

milestone_no <- 200

afltables %>% 
    select(season,round,date,playing_for,opp,id,first_name,surname,games_played) %>% 
    group_by(season,round,date,playing_for,opp) %>% 
    filter(any(games_played == milestone_no)) %>% 
    group_by(playing_for) %>% 
    mutate(t_milestones = sum(games_played == milestone_no)) %>% 
    group_by(playing_for, id, first_name, surname, t_milestones) %>% 
    summarise(
        n = n(),
        ratio = n()/t_milestones*100
    ) %>% View()

pend_view <- afltables %>% 
    select(season, round , id, first_name, surname, fantasy_points) %>% 
    filter(surname == 'Pendlebury')

ggplot(pend_view) +
    geom_point(aes(x = round, y = season, size = fantasy_points)) +
    xlab('') + ylab('') +
    scale_y_reverse() +
    # scale_size_area(max_size = 3) +
    scale_x_discrete()

afltables %>% 
    # select(season, round, date, playing_for, opp, inside_50_s) %>% 
    group_by(season, round, date, playing_for, opp, w_l) %>% 
    summarise(
        t_i50 = sum(inside_50_s)
    ) %>% View

afltables %>% 
    select(season, round, date, home_team, away_team, possessions, surname) %>%
    filter(possessions > 0) %>%
    group_by(season, round, home_team, away_team, possessions) %>% 
    summarise(
        n = n(),
        players = paste0(surname, collapse = ', ')
    ) %>% View()

afltables %>% 
    select(season, playing_for, opp, id, first_name, surname,goals, behinds,playing_for_score, opp_score) %>% 
    mutate(
        margin = playing_for_score - opp_score,
        player_score = goals*6+behinds,
        player_margin = player_score - opp_score
    ) %>% View()

afltables %>% select(playing_for,season,round,id, first_name, surname, disposals, marks) %>% 
    mutate(diff = disposals - marks) %>% View()

afltables %>% 
    select(season, round, date, playing_for, id, first_name) %>% 
    mutate(first_init = str_trunc(first_name, 1,'right',ellipsis = '')) %>% 
    group_by(season, round, date, playing_for, first_init) %>% 
    summarise(
        n = n(),
        names = paste0(first_name, collapse = ', ')
    ) %>% View()

afltables %>% 
    select(season, round, date, home_team, away_team) %>% 
    distinct() %>% 
    group_by(season, home_team, away_team) %>% 
    summarise(
        n = n(),
        rounds = paste0(round, collapse = ', ')
    ) %>% View()

finals_data <- afltables %>% 
    select(season, playing_for, id, first_name, surname, round, games_played, w_l) %>% 
    mutate(
        final_tf = !(round %in% 1:26),
        win_tf = w_l == 'W',
        final_win_tf = w_l == 'W' & !(round %in% 1:26)
    ) %>% 
    group_by(id) %>% 
    mutate(
        games_won = cumsum(win_tf),
        finals_played = cumsum(final_tf),
        finals_won = cumsum(final_win_tf)
    ) %>% ungroup() %>% 
    filter(season %in% 2010:2021) %>% 
    group_by(season, id) %>% 
    filter(games_played == max(games_played)) %>% 
    select(season, id, first_name, surname, games_played, games_won, finals_played, finals_won)

write.csv(finals_data,'finals_data.csv',row.names = F)

afltables %>% 
    select(season, date, playing_for_short, id, first_name, surname, games_played) %>% 
    filter(games_played %in% 1:31) %>%
    mutate(day_comp = day(date)) %>% 
    group_by(id, first_name, surname) %>% 
    # filter(any(season == 2022)) %>% 
    summarise(
        unique_days = n_distinct(day_comp),
        teams = paste0(unique(playing_for_short),collapse = ', '),
        seasons = paste0(min(season),'-',max(season)),
        cumul_played = max(games_played),
        diff = cumul_played - unique_days
    ) %>% arrange(-unique_days) %>% View()

afltables %>% 
    filter(w_l == 'D') %>% 
    group_by(id, first_name, surname) %>% 
    count() %>% View()

afltables %>% 
    group_by(id) %>% mutate(debut = min(date)) %>% 
    filter(debut > as.Date('1966-1-1')) %>% 
    select(date, debut, games_played, playing_for, id, first_name, surname, games_played, hit_outs, goals) %>%
    group_by(id, first_name, surname) %>% 
    mutate(
        cumul_ho = cumsum(hit_outs),
        cumul_goals = cumsum(goals),
        prev_ho = lag(cumul_ho),
        prev_goals = lag(cumul_goals)
    ) %>% ungroup() %>% 
    filter(cumul_ho > 0, cumul_goals > 0, prev_ho == 0, prev_goals == 0) %>% View()

sub_diff <- afltables %>% 
    filter(season == 2022) %>% 
    select(season, date, round, playing_for_short, id, first_name, surname, jumper_no_raw) %>% 
    mutate(subbed_off = grepl('↓',jumper_no_raw)) %>% 
    group_by(id) %>% 
    mutate(
        next_app = lead(date)
    ) %>% ungroup() %>% 
    filter(subbed_off == T) %>% #drop_na() %>% 
    mutate(diff = next_app - date)

write.csv(sub_diff,'sub_times.csv', row.names = F)

sub_not_return <- afltables %>% 
    filter(season == 2022) %>% 
    select(season, date, round, playing_for_short, id, first_name, surname, jumper_no_raw) %>% 
    mutate(subbed_off = grepl('↓',jumper_no_raw)) %>% 
    group_by(id) %>% 
    mutate(
        next_app = lead(date)
    ) %>% ungroup() %>% 
    filter(subbed_off == T, is.na(next_app), round %in% 1:25) %>% 
    group_by(playing_for_short) %>% count(name = 'not_returned')

ggplot(sub_diff) +
    geom_density(aes(x = diff, fill = playing_for_short))

library(gt)

sub_diff %>% 
    group_by(playing_for_short) %>% 
    summarise(
        avg_return = mean(diff) %>% round(2),
        median_return = median(diff) %>% as.numeric(),
        instant_return = sum(diff <= 10),
        n = n()
    ) %>% 
    left_join(sub_not_return) %>% 
    arrange(median_return) %>% 
    select(Team = playing_for_short, `Median Return Time (days)` = median_return,
           `No. Instant Returns` = instant_return,
           `No. Returned Players` = n, `No. Not Returned Players` = not_returned) %>% 
    gt() %>% 
    tab_header(
        title = "2022 Medi-Sub Rorting",
        subtitle = "How long do teams take to return players to the named 23?",
    ) %>% 
    cols_align(
        align = "center"
    ) %>% 
    data_color(
        columns = `Median Return Time (days)`, 
        colors = scales::col_numeric( 
            palette = c('red', 'orange', 'white'),
            domain = c(0, 40) 
        )
    ) %>% 
    data_color(
        columns = `No. Instant Returns`, 
        colors = scales::col_numeric( 
            palette = c('white', 'orange', 'red'),
            domain = c(0, 5) 
        )
    ) %>% 
    tab_source_note(
        source_note = "Does not include players subbed out in Round 19"
    ) %>%  
    tab_source_note(
        source_note = "No. Instant Returns is a count of players subbed out and return within 9 days"
    ) %>% 
    tab_options(table.font.size = 8)

afltables %>% 
    select(season, round, id, first_name, surname, playing_for_short, goals, games_played) %>% 
    group_by(id, first_name, surname) %>% 
    filter(games_played == max(games_played)) %>% View()
