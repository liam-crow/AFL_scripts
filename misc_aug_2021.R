library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
source("load_afltables.R")
source("fryzigg_data.R")

#Nick Beltrami

fryzigg_data %>% 
    select(season, date, id = player_id, first_name = player_first_name, surname = player_last_name, supercoach_score, fantasy_points) %>% 
    group_by(id, first_name, surname) %>% 
    filter(season == max(season)) %>% 
    summarise(
        season = unique(season),
        games = n(),
        avg_sc = mean(supercoach_score),
        avg_fp = mean(fantasy_points),
    ) %>% View()

# mayne 94 fantasy points

afltables %>% 
    select(id, first_name, surname) %>% distinct() %>% View()

# Bailley Baltrusaitis
# Hey, wondering if there's is a thing for a team 
# losing to the 2 Perth teams by the same amount in the same season?
# Richmond lost by 4 points to both this year.

afltables %>% 
    select(season, round, playing_for, opp, playing_for_score, opp_score) %>% 
    distinct() %>% 
    mutate(margin = playing_for_score - opp_score) %>% 
    filter(opp %in% c('West Coast', 'Fremantle')) %>% 
    group_by(season, playing_for, margin) %>% 
    summarise(
        n = n(),
        res = paste(opp,'r', round, collapse = ', ')
    ) %>% 
    View()

afltables %>% 
    select(season, round, playing_for, opp, playing_for_score, opp_score) %>% 
    distinct() %>% 
    mutate(margin = playing_for_score - opp_score) %>% 
    filter(opp %in% c('Brisbane Lions', 'Gold Coast')) %>% 
    group_by(season, playing_for, margin) %>% 
    summarise(
        n = n(),
        res = paste(opp,'r', round, collapse = ', ')
    ) %>% 
    View()

afltables %>% 
    select(season, round, playing_for, opp, playing_for_score, opp_score) %>% 
    distinct() %>% 
    mutate(margin = playing_for_score - opp_score) %>% 
    filter(opp %in% c('Port Adelaide', 'Adelaide')) %>% 
    group_by(season, playing_for, margin) %>% 
    summarise(
        n = n(),
        res = paste(opp,'r', round, collapse = ', ')
    ) %>% 
    View()

afltables %>% 
    select(season, round, playing_for, opp, playing_for_score, opp_score) %>% 
    distinct() %>% 
    mutate(margin = playing_for_score - opp_score) %>% 
    filter(opp %in% c('Greater Western Sydney', 'Sydney')) %>% 
    group_by(season, playing_for, margin) %>% 
    summarise(
        n = n(),
        res = paste(opp,'r', round, collapse = ', ')
    ) %>% 
    View()


# Cameron Jones
# Hey team, love your work! Just wondering if you have 
# a stat for the most a team has made up in the final quarter to still lose?
# So if a team was down by 60 and lost by 10, 
# they made up 50 but still lost.
# What’s the biggest margin to make up that 
# didn’t come away with the win

afltables %>% 
    select(season, round, playing_for, opp, pq_3_g, pq_3_b, oq_3_g, oq_3_b, playing_for_score, opp_score) %>% 
    mutate(
        margin = playing_for_score - opp_score,
        q3_margin = pq_3_g * 6 + pq_3_b - oq_3_g *6 - oq_3_b,
        margin_diff = margin - q3_margin
    ) %>% 
    filter(
        margin < 0,
        q3_margin < 0
    ) %>% 
    distinct() %>% View()

# Silas Weare

# Found the podcast last week boys and loving it on 
# the way too and from work, very useless indeed! 
# Just wondering if anyone’s explored the idea 
# of what the ladder would look like if the winning 
# team received their margin as points, e.g. Dees 
# getting 98 points after they beat the Suns. 
# Would we see any past premiers missing out on 
# finals if this was the case? Something to chew 
# on if you get bored, stay useless

afltables_custom <- afltables %>% 
    select(season, round, date, playing_for, playing_for_score, opp_score) %>% 
    mutate(
        w_l = case_when(
            playing_for_score > opp_score ~ 'W',
            playing_for_score < opp_score ~ 'L',
            T ~ 'D',
        )
    ) %>% 
    filter(
        season == 2021, 
        !round %in% c('EF','GF','PF','QF','SF')
    ) %>% distinct()

form_custom <- afltables_custom %>% 
    group_by(playing_for, w_l) %>% 
    summarise(
        s_score = sum(playing_for_score),
        s_opp_score = sum(opp_score),
        games = n(),
        .groups = 'drop'
    )

custom_ladder <- form_custom %>% 
    pivot_wider(
        names_from = w_l,
        values_from = games,
        values_fill = list(games = 0)
    ) %>%
    group_by(playing_for) %>% 
    summarise(
        W = sum(W),
        D = sum(D),
        L = sum(L),
        P = sum(s_score - s_opp_score),
        .groups = 'drop'
    ) %>% arrange(-P) # + D*2

write.csv(custom_ladder, 'aflxepl_2020.csv')
View(custom_ladder)



afltables %>% 
    select(season, round, playing_for, playing_for_score, w_l) %>% distinct() %>% 
    filter(playing_for_score >= 100, w_l == 'L') %>% 
    group_by(season, playing_for) %>% 
    summarise(
        n = n()
    ) %>% View()

afltables %>% 
    select(season, date, id, first_name, surname, opp) %>% 
    group_by(id, first_name, surname) %>% 
    mutate(m_season = max(season)) %>% 
    filter(m_season == 2021) %>% 
    summarise(
        games_played = n(),
        n_unique_teams = length(unique(opp)),
        teams = paste(unique(opp), collapse = ', ')
    ) %>% View()

afltables %>% 
    select(season, date, round, id, first_name, surname, games_played, venue) %>%
    filter(venue == 'M.C.G.') %>% 
    group_by(id, first_name, surname) %>% 
    filter(date == min(date)) %>% 
    View()

afltables %>% 
    select(season, round, date, playing_for, playing_for, id, first_name, surname) %>% 
    filter(substring(first_name,1,1) == substring(surname,1,1)) %>%
    group_by(season, round, date, playing_for) %>% 
    summarise(
        n = n(),
        names = paste(first_name, surname, collapse = ', ')
    ) %>% View()

# Tommy Lucin
# What’s the most amount of players with 
# Mc in their surname in one team?

# Brisbane Lions McStay, McCarthy, McLuggage, McInerney, McCreary and Macrae
afltables %>% 
    select(season, round, date, home_team, away_team, playing_for, id, first_name, surname) %>% 
    filter(grepl('^mc|^mac', surname, ignore.case = T)) %>%
    group_by(season, round, date, home_team, away_team) %>% 
    summarise(
        comb = paste(surname, collapse = ', '),
        n = n()
    ) %>% View()

goals_in_a_row <- NULL
for (i in 1:24) {
    
    goals_in_a_row_round <- afltables %>% 
        filter(season >= 1965, round %in% 1:i) %>%
        select(season, round, date, id, first_name, surname, goals, behinds) %>% 
        group_by(season, id, first_name, surname) %>% 
        summarise(
            t_goals = sum(goals),
            t_behinds = sum(behinds),
            n = n(),
            .groups = 'drop'
        ) %>% 
        filter(t_behinds == 0) %>% 
        arrange(-t_goals) %>% head(5) %>% 
        mutate(round = i)
    goals_in_a_row <- rbind(goals_in_a_row, goals_in_a_row_round)
}

# goals_in_a_row_round <- afltables %>% 
#     filter(season >= 1965) %>%
#     select(season, round, date, id, first_name, surname, goals, behinds) %>% 
#     group_by(season, id, first_name, surname) %>% 
#     summarise(
#         t_goals = sum(goals),
#         t_behinds = sum(behinds),
#         n = n(),
#         .groups = 'drop'
#     ) %>% 
#     filter(t_behinds == 0) %>% 
#     arrange(-t_goals) %>% head(5) %>% 
#     mutate(round = 'all')
# goals_in_a_row <- rbind(goals_in_a_row, goals_in_a_row_round)
goals_in_a_row %>% View()

# Rhys Mathewson
# What’s the most goals in a row to start a career without a behind ?
# Ginnivan for pies has 6-0 and made me interested

games_played_goals <- afltables %>% 
    filter(season >= 1965) %>% 
    select(id, first_name, surname, games_played, goals, behinds)

games_played_goals_total <- NULL
for (i in 1:100) {
    games_played_goals_ind <- games_played_goals %>% 
        filter(games_played %in% c(1:i)) %>% 
        group_by(id, first_name, surname) %>% 
        summarise(
            t_goals = sum(goals),
            t_behinds= sum(behinds),
            .groups = 'drop'
        ) %>% 
        filter(t_behinds == 0) %>% 
        arrange(-t_goals) %>% head(5) %>% 
        mutate(games_played = i)
    games_played_goals_total <- rbind(games_played_goals_total, games_played_goals_ind)
}

View(games_played_goals_total)

#Ryan Fitzgerald, Will Snelling and Zach Merrett 
# went 11.0 (complete games) before kicking a point

#Ethan Keele
# joe daniher has about 35 to 40% of 
# the total Brisbane lions bounces this 
# season. is this the most by a player?

afltables %>% 
    select(season, round, playing_for, id, first_name, surname, bounces) %>% 
    group_by(season, playing_for) %>% 
    mutate(t_bounces = sum(bounces)) %>% 
    group_by(season, id, first_name, surname) %>% 
    mutate(t_ind_bounces = sum(bounces)) %>% ungroup() %>% 
    mutate(ratio = round(t_ind_bounces/t_bounces*100),2) %>% 
    select(season, playing_for, id, first_name, surname, ratio, t_bounces, t_ind_bounces) %>% 
    distinct() %>% View()

afltables %>% 
    select(season, round, playing_for, opp, starts_with('pq'), w_l) %>% 
    distinct() %>% 
    filter(
        pq_1_g == pq_1_b,
        pq_2_g == pq_2_b,
        pq_3_g == pq_3_b,
        pq_4_g == pq_4_b,
    ) %>% View()

# Anthony Duke
# What’s the most a player had lost by 
# and been chaired off and/or retired at the same time ?

afltables %>% 
    select(season, round, playing_for_score, opp_score, id, first_name, surname, games_played) %>% 
    group_by(id, first_name, surname) %>% 
    filter(games_played == max(games_played)) %>% 
    mutate(margin = playing_for_score - opp_score) %>% View()

afltables %>% 
    select(season, round, date, playing_for, opp, w_l) %>% distinct() %>% 
    filter(w_l == "W") %>% 
    group_by(playing_for, opp) %>% 
    filter(date == max(date)) %>% ungroup() %>% 
    mutate(
        diff_date = Sys.Date() - date,
        comb = paste0(playing_for,' vs ',opp,' ',diff_date,' days (',season,' r',round,')')
    ) %>% View()

# James Otto
# Hey guys! I was thinking if Melbourne play 
# geelong in the first week of finals and the 
# grand final they could play each other 4 
# times this calendar year - think this has 
# happened a few times but has a team ever won all 4?
# Thanks!

afltables %>% 
    select(season, round, date, playing_for, opp, w_l) %>% 
    filter(season >= 1965, w_l == 'W') %>% 
    distinct() %>% 
    group_by(season, playing_for, opp) %>% 
    summarise(
        n = n(),
        comb = paste(round, collapse = ', ')
    ) %>% View()

#David Bravos
# Hey team, for freo yesterday there 
# was Monday playing game 353. Next 
# most experienced player was 
# Colyer in game 128. Is that the 
# biggest diff between most and 2nd 
# most experienced players on a team sheet?

afltables %>% 
    select(date, season, round, playing_for, id, first_name, surname, games_played) %>% 
    group_by(date, season, round, playing_for) %>% 
    arrange(desc(games_played)) %>% 
    slice(1:2) %>% 
    summarise(
        diff = max(games_played) - min(games_played),
        players = paste(first_name, surname, games_played, collapse = ', ')
    ) %>% View()

# Owen Missen
# Hey guys, when did a team last score 
# 63% or more of their total score in 
# the first qtr as Freo did?

afltables %>% 
    filter(season > 1964) %>% 
    select(season, round, date, playing_for, pq_1_g, pq_1_b, playing_for_score, w_l) %>% distinct() %>% 
    mutate(
        q1_score = pq_1_g*6 + pq_1_b,
        q1_score_ratio = round(q1_score/playing_for_score*100,2)
    ) %>% View()

afltables %>% 
    select(date, season, round, playing_for, w_l) %>% distinct() %>% 
    filter(playing_for %in% c('Geelong','Port Adelaide','Fremantle'), w_l == 'W') %>% 
    group_by(season, round) %>% 
    summarise(
        n = n()
    ) %>% View()


afltables %>% 
    select(playing_for, id, first_name, surname, w_l) %>% 
    group_by(id, first_name, surname) %>% 
    mutate(n_teams = length(unique(playing_for))) %>% 
    group_by(playing_for, id, first_name, surname) %>% 
    mutate(games_per_team = n()) %>% 
    filter(w_l == 'W') %>% 
    mutate(wins_per_team = n()) %>% ungroup() %>% 
    filter(games_per_team >= 50, n_teams == 2) %>% 
    select(playing_for, id, first_name, surname, n_teams, wins_per_team, games_per_team) %>% 
    distinct() %>% 
    mutate(win_ratio = round(wins_per_team/games_per_team*100,2)) %>% View()
group_by(id, first_name, surname, n_teams) %>% 
    summarise(
        n_teams_i = length(unique(playing_for)),
        win_ratio_diff = max(win_ratio) - min(win_ratio),
        max_win_r = max(win_ratio),
        min_win_r = min(win_ratio)
    ) %>% View()

afltables %>% 
    select(season, round, date, id, first_name, surname, tackles, frees_for) %>% 
    View()

id_name <- unique(afltables$first_name)

afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname) %>% 
    filter(surname %in% id_name) %>% 
    group_by(season, round, date, playing_for) %>% 
    summarise(
        n = n(),
        comb = paste(first_name, surname, collapse = ', ')
    ) %>% View()

afltables %>% 
    filter(season > 1964) %>% 
    select(date, season, round, home_team, away_team, jumper_no) %>% 
    group_by(date, season, round, home_team, away_team) %>% 
    summarise(
        low_jumper = min(jumper_no),
        high_jumper = max(jumper_no),
        diff = high_jumper - low_jumper
    ) %>% View()

# Alastair Wills
# Eg Bont plays Dusty, both wear no 4, that 
# counts as 1 number used. What is the most/fewest 
# numbers to appear in a game

afltables %>% 
    filter(season > 1964) %>% 
    select(date, season, round, home_team, away_team, jumper_no) %>% 
    distinct() %>% 
    group_by(date, season, round, home_team, away_team) %>% 
    summarise(n = n()) %>% View()

afltables %>% 
    select(date, season, round, playing_for, id, first_name, surname, goals, games_played) %>% 
    filter(playing_for == 'Richmond', games_played == 1, goals >= 3) %>% 
    View()


