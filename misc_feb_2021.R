library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
source("load_afltables.R")
source("fryzigg_data.R")

fitzRoy::get_aflw_detailed_data()

cookie <- get_aflw_cookie()

aflw_player_stats <- get_aflw_player_stats()

glimpse(aflw_player_stats)

aflw_player_stats %>% 
    select(date, fixture_round, player_name, fantasy_score, metres_gained) %>% 
    mutate(mpf = metres_gained/fantasy_score) %>% 
    filter(mpf != Inf, fantasy_score > 10) %>% 
    View()

tackle_data <- fryzigg_data %>% 
    select(season, player_position, intercept_marks, player_height_cm) %>% 
    mutate(
        position_grouped = case_when(
            player_position == 'BPL' ~ 'Backs',
            player_position == 'BPR' ~ 'Backs',
            player_position == 'FB' ~ 'Backs',
            player_position == 'CHB' ~ 'Backs',
            player_position == 'HBFL' ~ 'Backs',
            player_position == 'HBFR' ~ 'Backs',
            player_position == 'FPL' ~ 'Forwards',
            player_position == 'FPR' ~ 'Forwards',
            player_position == 'FF' ~ 'Forwards',
            player_position == 'CHF' ~ 'Forwards',
            player_position == 'HFFL' ~ 'Forwards',
            player_position == 'HFFR' ~ 'Forwards',
            player_position == 'C' ~ 'Centres',
            player_position == 'R' ~ 'Centres',
            player_position == 'RK' ~ 'Centres',
            player_position == 'RR' ~ 'Centres',
            player_position == 'WL' ~ 'Centres',
            player_position == 'WR' ~ 'Centres',
            TRUE ~ 'other'
        )
    ) %>% 
    drop_na() %>% 
    filter(season == 2020, player_position != 'INT')

library(ggplot2)

ggplot(tackle_data, aes(y = player_height_cm, x = as.character(intercept_marks))) +
    geom_violin() + geom_point() + geom_jitter() +
    geom_smooth(aes(y = player_height_cm, x = intercept_marks), method = 'gam')

# geom_density2d()

ggplot(fryzigg_data %>% filter(season == 2020), aes(x = as.character(free_kicks_for), y = player_height_cm)) +
    geom_violin() + geom_jitter(alpha = 0.1) 

ggplot(fryzigg_data %>% filter(season == 2020), aes(x = as.character(free_kicks_against), y = player_height_cm)) +
    geom_violin() + geom_jitter(alpha = 0.1) + 
    geom_smooth(inherit.aes = F, aes(x = free_kicks_against, y = player_height_cm), method = 'lm')

fryzigg_data %>% 
    select(date,season,match_round,match_winner, player_id, player_first_name, player_last_name, player_position, pressure_acts, def_half_pressure_acts, intercept_marks, turnovers, clangers, one_percenters) %>% 
    drop_na() %>% 
    mutate(fwd_half_pressure_acts = pressure_acts - def_half_pressure_acts) %>% 
    filter(fwd_half_pressure_acts == def_half_pressure_acts) %>% View()

afltables %>% 
    select(season, round, venue, playing_for, opp, playing_for_score, opp_score) %>% distinct() %>% 
    filter(venue == 'Gabba', playing_for_score == 131)

afltables %>% 
    select(season, round, home_team, away_team, playing_for, playing_for_score, opp_score) %>% distinct() %>% 
    group_by(playing_for_score) %>% 
    count() %>% View()


#### palindromes ####
afltables %>% 
    select(date, round, home_team, away_team, home_score, away_score) %>% distinct() %>% 
    rowwise() %>% 
    mutate(
        combined = paste0(home_score, away_score),
        backwards  = intToUtf8(rev(utf8ToInt(combined))),
        sum = sum(home_score, away_score)
    ) %>% 
    filter(combined == backwards) %>% View()

afltables %>% 
    select(date, round, id, first_name, surname, kicks, handballs) %>% distinct() %>% 
    rowwise() %>% 
    mutate(
        combined = paste0(kicks, handballs),
        backwards  = intToUtf8(rev(utf8ToInt(combined))),
        sum = sum(kicks, handballs)
    ) %>% 
    filter(combined == backwards, sum > 0) %>% View()

afltables %>% 
    select(date, round, playing_for, pq_4_g, pq_4_b, playing_for_score) %>% distinct() %>% 
    rowwise() %>% 
    mutate(
        combined = paste0(pq_4_g, pq_4_b, playing_for_score),
        backwards  = intToUtf8(rev(utf8ToInt(combined)))
    ) %>% 
    filter(combined == backwards) %>% View()

afltables %>% 
    select(date, round, playing_for, pq_1_g, pq_1_b, oq_1_g, oq_1_b) %>% distinct() %>% 
    rowwise() %>% 
    mutate(
        pq_score = pq_1_g*6 + pq_1_b,
        oq_score = oq_1_g*6 + oq_1_b,
        combined = paste0(pq_1_g, pq_1_b, pq_score, oq_1_g, oq_1_b, oq_score),
        backwards  = intToUtf8(rev(utf8ToInt(combined)))
    ) %>% 
    filter(combined == backwards) %>% View()

afltables %>% 
    select(date, round, id, first_name, surname, disposals) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(t_disp = sum(disposals), .groups = 'keep') %>% 
    rowwise() %>% 
    mutate(
        combined = paste0(t_disp),
        backwards  = intToUtf8(rev(utf8ToInt(combined)))
    ) %>% 
    filter(combined == backwards) %>% View()

afltables %>% 
    select(season, round, home_team, away_team, hq_4_g, hq_4_b, aq_4_g, aq_4_b, home_score, away_score) %>% distinct() %>% 
    filter(hq_4_g == aq_4_b, hq_4_b == aq_4_g) %>% 
    mutate(margin = home_score - away_score) %>% View()

afltables %>% 
    filter(season >= 1965) %>% 
    select(id, first_name, surname, kicks, handballs) %>% 
    mutate(
        foot = grepl('foot', surname, ignore.case = T),
        hand = grepl('hand', surname, ignore.case = T)
    ) %>% 
    group_by(foot, hand) %>% 
    summarise(t_kicks = sum(kicks), t_hballs = sum(handballs))

afltables %>% 
    select(season, round, playing_for, starts_with('pq')) %>% distinct() %>% 
    mutate(
        q1 = pq_1_g*6 + pq_1_b,
        q2 = pq_2_g*6 + pq_2_b,
        q3 = pq_3_g*6 + pq_3_b,
        q4 = pq_4_g*6 + pq_4_b
    ) %>% #View()
    filter()

fryzigg_data %>% 
    filter(season >= 2021) %>% 
    select(player_id, player_first_name, player_last_name, player_height_cm, player_weight_kg) %>% distinct() %>% drop_na() %>% 
    inner_join(
        starwars %>% select(name, height, mass), 
        by = c('player_height_cm' = 'height', 'player_weight_kg' = 'mass')
    ) %>% 
    summarise(
        comb = paste0(player_first_name,' ',player_last_name,' & ',name,': ',player_height_cm,'cm, ',player_weight_kg,'kg', collapse = '')
    ) %>% View()

afltables %>% 
    select(date, home_team, away_team) %>% distinct() %>% 
    mutate(m = month(date), d = day(date)) %>% 
    group_by(d, m) %>% 
    summarise(n = n(), years = paste(year(date), home_team, away_team, collapse = ', ')) %>% 
    arrange(m, d) %>% View()

afltables %>% 
    select(date, first_name, surname) %>% #View()
    group_by(first_name) %>% 
    arrange(date) %>% 
    summarise(diff = date - lag(date)) #%>% View()

afltables %>% 
    select(season, date, round, playing_for, opp, frees_for, frees_against)

sample_data <- afltables %>% 
    select(date, round, home_team, away_team, team = playing_for, kicks, handballs, tackles, inside_50_s, cont_pos = contested_possessions, uncont_pos = uncontested_possessions, home_score, away_score) %>% 
    group_by(date, round, home_team, away_team) %>% 
    mutate(game_id = cur_group_id()) %>% ungroup() %>% 
    mutate(h_a = if_else(home_team == team, "H", "A")) %>% 
    group_by(game_id, date, round, team, h_a, home_score, away_score) %>% 
    summarise(
        s_kicks = sum(kicks),
        s_handballs = sum(handballs),
        s_tackles = sum(tackles),
        s_inside_50_s = sum(inside_50_s),
        s_cont_pos = sum(cont_pos),
        s_uncont_pos = sum(uncont_pos),
    ) %>% arrange(game_id, desc(h_a)) %>% ungroup() %>% 
    pivot_wider(
        names_from = c(h_a),
        values_from = c(team, s_kicks, s_handballs, s_tackles, s_inside_50_s, s_cont_pos, s_uncont_pos)
    ) %>% 
    mutate(
        score_diff = home_score - away_score,
        kicks_diff = s_kicks_H - s_kicks_A,
        handballs_diff = s_handballs_H - s_handballs_A,
        tackles_diff = s_tackles_H - s_tackles_A,
        inside_50_s_diff = s_inside_50_s_H - s_inside_50_s_A,
        cont_pos_diff = s_cont_pos_H - s_cont_pos_A,
        uncont_pos_diff = s_uncont_pos_H - s_uncont_pos_A,
        .keep = "unused"
    )

afltables %>% 
    select(season, round, home_team, away_team, first_name, surname) %>% 
    group_by(season, round, home_team, away_team, first_name) %>% 
    summarise(n = n()) %>% View()

afltables %>% 
    select(season, round, home_team, away_team, playing_for, pq_1_g, pq_2_g, pq_3_g, pq_4_g) %>% 
    distinct() %>% 
    mutate(g1 = pq_1_g, g2 = pq_2_g - pq_1_g, g3 = pq_3_g - pq_2_g, g4 = pq_4_g - pq_3_g) %>% 
    filter(
        g1 < g2,
        g2 < g3,
        g3 < g4,
        g1 >= 4
    ) %>% arrange(season) %>% View()

afltables %>% 
    select(season, date, round, home_team, away_team, playing_for, kicks, handballs) %>% 
    mutate(disp = kicks + handballs) %>% 
    filter(disp >= 9) %>% 
    group_by(season, date, round, home_team, away_team, playing_for) %>% 
    summarise(n = n()) %>% View()

library(dplyr)
library(tidyr)
library(snakecase)
library(fitzRoy)

afltables <- fetch_player_stats_afltables(season = 2021)

dim(afltables)

head(afltables)

names(afltables) <- to_snake_case(names(afltables))

#team names = Richmond, Carlton, Collingwood, Western Bulldogs, Melbourne, 
#Fremantle, Adelaide, Geelong, Essendon, Hawthorn, Brisbane Lions, Sydney 
#North Melbourne, Port Adelaide, Greater Western Sydney, St Kilda, West Coast
#Gold Coast, Carlton, 

useful_stats <- afltables %>% 
    select(round, first_name, surname, playing_for, kicks, handballs, goals, time_on_ground) %>% 
    #mutate(games_played = ) %>% 
    group_by(playing_for, first_name, surname)  %>% 
    summarise(
        GP = sum(time_on_ground > 0),
        avgD = mean(kicks+handballs),
        pcnt_20_plus_D = ((sum((kicks + handballs)>=20))/(sum(time_on_ground > 0)))*100,
        pcnt_25_plus_D = ((sum((kicks + handballs)>=25))/(sum(time_on_ground > 0)))*100,
        avgG = mean(goals),
        pcnt_1_plus_G = ((sum((goals)>=1))/(sum(time_on_ground > 0)))*100,
        pcnt_1_plus_G = ((sum((goals)>=2))/(sum(time_on_ground > 0)))*100,
        .groups = 'drop'
    ) %>% 
    arrange(playing_for, surname)
View(useful_stats)

afltables %>% 
    select(season, round, date, home_team, away_team, playing_for, first_name, surname, disposals) %>% 
    group_by(season, round, date, home_team, away_team, playing_for, first_name) %>% 
    summarise(
        n = n(),
        s_disp = sum(disposals)
    ) %>% 
    filter(n >= 2) %>% View()

afltables %>% 
    select(season, date, round, playing_for, opp, pq_2_g, pq_2_b, oq_2_g, oq_2_b, w_l) %>% distinct() %>% 
    mutate(
        HT_p = pq_2_g*6 + pq_2_b,
        HT_o = oq_2_g*6 + oq_2_b,
        ratio = HT_p/HT_o
    ) %>% 
    filter(ratio >= 2, w_l %in% c('L','D')) %>% View()

afltables
lubridate::wday('2021/4/14', label= T) %>% as.character()

thu_stk_rich <- afltables %>% 
    select(season, date, playing_for, id, first_name, surname) %>% 
    filter(playing_for %in% c('Richmond', 'St Kilda')) %>% 
    mutate(day_of_week = as.character(lubridate::wday(date, label= T))) %>% 
    group_by(day_of_week, id, first_name, surname) %>% 
    summarise(
        n = n(),
        teams = paste(unique(playing_for), collapse = ', ')
    ) %>% 
    filter(day_of_week == 'Thu')

write.csv(thu_stk_rich, file = 'thu_stk_rich.csv', row.names = F)

afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname, disposals, marks) %>% 
    filter(marks == 0, disposals >=1) %>% View()

afltables %>% 
    select(season, round, date, playing_for, opp, playing_for_score, opp_score) %>% distinct() %>% 
    mutate(
        day_of_week = as.character(lubridate::wday(date, label= T)),
        margin = playing_for_score-opp_score
    ) %>% 
    filter(day_of_week == 'Thu') %>% View()

frees_data <- afltables %>% 
    select(season, round, date, playing_for, frees_for, frees_against, playing_for_score, opp_score) %>% 
    group_by(season, round, date, playing_for) %>% 
    summarise(
        sfrees_for = sum(frees_for),
        sfrees_against = sum(frees_against),
        playing_for_score = unique(playing_for_score),
        opp_score = unique(opp_score),
        .groups = 'drop'
    ) %>% 
    filter(
        season > 2000,
        sfrees_for > 0 & sfrees_against > 0
    ) %>% 
    mutate(
        diff = sfrees_for - sfrees_against,
        margin = playing_for_score - opp_score
    )
View(frees_data)

library(ggplot2)
ggplot(frees_data) + 
    geom_point(aes(x = diff, y = margin))

frees_data %>% 
    filter(sfrees_for > playing_for_score) %>% View()

afltables %>% 
    select(season, round, date, playing_for, opp, playing_for_score, opp_score) %>% 
    distinct() %>% 
    mutate(margin = playing_for_score - opp_score) %>% View()

afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname) %>% 
    group_by(season, round, date, first_name, surname) %>% 
    summarise(
        n = length(unique(playing_for)),
        teams = paste(unique(playing_for), collapse = ', ')
    ) %>% 
    filter(n != 1) %>% 
    group_by(first_name, surname) %>% count() %>% 
    View()

names(fryzigg_data)
fryzigg_data %>% 
    select(match_date, match_round, match_home_team, match_away_team, player_id, player_first_name, player_last_name, metres_gained) %>% View()

afltables %>% 
    select(season, round, date, playing_for, opp, pq_1_g, pq_1_b, oq_1_g, oq_1_b, w_l, playing_for_score, opp_score) %>% distinct() %>% 
    mutate(
        margin = playing_for_score - opp_score,
        p_q1 = pq_1_g*6 + pq_1_b,
        o_q1 = oq_1_g*6 + oq_1_b,
        q1_w_l = case_when(
            p_q1 > o_q1 ~ "W",
            p_q1 < o_q1 ~ "L",
            T ~ 'D'
        )
    ) %>% 
    filter(q1_w_l %in% c("L")) %>% 
    group_by(w_l) %>% summarise(n = n())
summarise(avg_marg = mean(margin))
# group_by(season, round) %>% count() %>% View()

