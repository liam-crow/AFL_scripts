library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
source("load_afltables.R")
source("fryzigg_data.R")

# Norman Jupp III
# Most different jumpers worn by a player ?? Burgoyne would have to be up there
afltables %>% 
    select(season, id, first_name, surname, jumper_no) %>% 
    filter(season > 1966) %>% 
    distinct() %>% 
    group_by(id, first_name, surname) %>% 
    count() %>% arrange(-n)

# Adam Matthews
# what is the most amount of bounces taken in one game, by a single player and by a team
afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname, bounces) %>% View()

# Gimhana James Bangagama
# How many times has richmond lost after being up by 20+ at 3 quarter time since 207

afltables %>% 
    select(season, round, date, playing_for, opp, pq_3_g, pq_3_b, oq_3_g, oq_3_b, playing_for_score, opp_score, w_l) %>% distinct() %>% 
    mutate(
        pq3 = pq_3_g * 6 + pq_3_b,
        oq3 = oq_3_g * 6 + oq_3_b,
        q3_margin = pq3 - oq3,
        fin_margin = playing_for_score - opp_score
    ) %>% 
    filter(q3_margin > 20, w_l == 'L') %>% View()

# Tom Bury asked - Davis subs on for Doedee, both have identical stat lines. 
# A first? Stats being 5 kicks 2 handballs 4 marks 1 tackle.

fryzigg_data %>% 
    select(season, match_round, match_home_team, match_away_team, player_team, player_id, player_first_name, player_last_name, kicks, handballs, marks, tackles, subbed) %>% 
    filter(season > 2012, subbed %in% c("Subbed In", "Subbed Out")) %>% 
    group_by(season, match_round, match_home_team, match_away_team, kicks, handballs, marks, tackles) %>% 
    summarise(
        n = n(),
        comb = paste(player_first_name, player_last_name, player_team, subbed, collapse = ', ')
    ) %>% View()

# Jackson Lowe - Was wondering if you guys knew the most wins a team has had and still 
# finished with the wooden spoon? And if Norf are a chance to beat it this year?

afltables %>% 
    select(season, round, date, playing_for, w_l) %>% distinct() %>% 
    filter(round %in% c(1:25)) %>% 
    mutate(points = 
               case_when(
                   w_l == 'W' ~ 4,
                   w_l == 'D' ~ 2,
                   w_l == 'L' ~ 0
               )
    ) %>% 
    group_by(season, playing_for) %>% 
    summarise(total_points = sum(points)) %>% 
    group_by(season) %>% 
    filter(total_points == min(total_points)) %>% View()

# Riley Dolman asked Who would be the player who has the the highest margin of a win 
# having a continuous set of margins i.e. starting at a draw then going 1,2,3 etc

afltables %>% 
    select(season, date, playing_for, opp, id, first_name, surname, playing_for_score, opp_score) %>% distinct() %>% 
    mutate(margin = playing_for_score - opp_score) %>% 
    arrange(date) %>%
    group_by(id, first_name, surname) %>% 
    mutate(margin_diff = margin == lag(margin)+1) %>% 
    filter(id == 558) %>% View()
summarise(
    consecutive = rle_tbl(margin_diff)
) %>% 
    View()

#Pætrick McMeagher
# How many players since 1990 have had NO bounces, in their entire career?

afltables %>% 
    select(season, id, first_name, surname, bounces, disposals) %>% 
    filter(season > 1999) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        sum_b = sum(bounces),
        sum_d = sum(disposals),
        games_played = n()
    ) %>% 
    filter(sum_b == 1) %>% 
    View()

#Benn Pollock
# How many players have won games by a margain that equals their jumper number?

afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname, jumper_no, playing_for_score, opp_score, w_l) %>% 
    mutate(margin = abs(playing_for_score - opp_score)) %>% 
    filter(jumper_no == margin, jumper_no > 0, w_l == "W") %>% 
    group_by(id, first_name, surname, jumper_no) %>% 
    count() %>% View()

#Chris Short
# Jack Graham only player to win when his team scores his jumper number?

afltables %>% 
    select(season, round, date, playing_for, opp, id, first_name, surname, jumper_no, playing_for_score, opp_score, w_l) %>% 
    filter(jumper_no == playing_for_score) %>% View()

# Andy Munro
# Who has bounced the least under 183cm

fryzigg_data %>% 
    select(player_id, player_first_name, player_last_name, player_height_cm, bounces) %>% 
    filter(player_height_cm <= 183) %>% 
    group_by(player_id, player_first_name, player_last_name, player_height_cm) %>% 
    summarise(sum_b = sum(bounces), n = n()) %>% 
    View()

afltables %>% 
    select(season, date, round, home_team, away_team) %>% 
    distinct() %>% 
    mutate(
        day = lubridate::wday(date, label = T),
        week= lubridate::week(date)
    ) %>% 
    group_by(season, round, week, day) %>% 
    count() %>% 
    group_by(day, n) %>% 
    count() %>% View()

afltables %>% 
    select(date, season, round, home_team, away_team, hq_4_b, hq_4_g, home_score, aq_4_g, aq_4_b, away_score) %>% 
    distinct() %>% 
    filter(hq_4_b == hq_4_g, aq_4_b == aq_4_g) %>% 
    mutate(home_score + away_score) %>% 
    View()

# Alex Diederich
# Lowest standard deviation in winning AFL scores in a round?
afltables %>% 
    select(season, round, date, home_team, away_team, playing_for_score, w_l) %>% 
    distinct() %>% 
    filter(w_l == 'W') %>% 
    filter(round %in% c(1:25)) %>% 
    group_by(season, round) %>% 
    summarise(
        scores = paste(playing_for_score, collapse = ', '),
        sd = sd(playing_for_score),
        n = n()
    ) %>% View()

afltables %>% 
    select(season, round, date, home_team, away_team, pq_2_g, pq_2_b, oq_2_g, oq_2_b) %>% 
    distinct() %>% 
    group_by(season, round, pq_2_g, pq_2_b, oq_2_g, oq_2_b) %>% 
    count() %>% View()
# Dylan Nossek
# Hi useless AFL stats,

# Here's an interesting stat I picked up: 
# in round 5 this season, Melbourne defeated 
# Hawthorn 104-54, then in round 18, they 
# drew 79-79, and it just so happens that 
# 79 is the average of 104 and 54. Has 
# something like this ever occurred before 
# (between the same two teams in the same season)?

afltables %>% 
    select(season, round, playing_for, opp, playing_for_score, opp_score, w_l) %>% 
    distinct() %>% 
    mutate(average_score = (playing_for_score + opp_score)/2) %>%
    group_by(season, playing_for, opp, average_score) %>% 
    summarise(
        comb = paste(w_l, collapse = ', '),
        n = n()
    ) %>% View()

# There have been 66 occurrences of paired 
# teams having the same average score 
# across 2+ games, most recently Richmond 
# and Brisbane in 2020 (averaging 61.5 
# in each of their 2 games (1W 1L)). 
# However Hawthorn Melbourne are the 
# only team to do so with a win and a draw

fryzigg_data %>% 
    select(season, date, match_round, player_team, 
           player_id, player_first_name, player_last_name, metres_gained, disposals) %>% 
    distinct() %>% drop_na() %>% 
    group_by(season, date, match_round, player_team, metres_gained) %>% 
    summarise(
        n = n(),
        names = paste(player_first_name, player_last_name, disposals, collapse = ', '),
        diff = max(disposals) - min(disposals)
    ) %>% View()

fryzigg_data %>% 
    select(season, date, player_team, player_id, player_first_name, player_last_name, spoils) %>% View()

afltables %>% 
    select(season, round, id, first_name, surname, clangers, disposals) %>% 
    View()

#### 26/7/2021 mailbag####

# Steve Fry
# Hi team. Just noticed that the Bulldogs have 
# not had an even numbered total kicked against 
# them since round 5, and 16 of the 18 scores 
# against them this year are odd. 
# Can you get some sort of useless stat out of that?


afltables %>% 
    select(season, round, playing_for, opp, playing_for_score, opp_score) %>% 
    distinct() %>% 
    mutate(odd_tf = opp_score %% 2 == 1) %>% 
    group_by(season, playing_for) %>% 
    mutate(games = n()) %>% 
    group_by(season, playing_for, games, odd_tf) %>% 
    summarise(n = n()) %>% 
    mutate(ratio = (n/games)*100) %>% View()

# bulldogs are have played against an opposition scoring an odd number 83% of the time,
# set to be a new record if they can keep it up (16/18 games against an odd score)
# next highest is 1962 Essendon 80% (16/18 games) against odd score
    
# 1938 Hawthorn and 1950 Carlton both 15/18 (83%) games were against even numbers


# Harry Lagastes
# Which AFL player have played 1 AFL game 
# but for the biggest crowd in that one game
afltables %>% 
    select(season, round, playing_for, opp, attendance, id, first_name, surname, games_played) %>% 
    group_by(id, first_name, surname) %>% 
    filter(games_played == max(games_played), games_played == 1) %>% View()

# Tom Jok (2019 r23 Ess vs Coll) in front of 85405 
# is the record, ahead of Denis O'Brien 
# (1971 r6 Coll vs Melb) in front of 80231

fryzigg_data %>% 
    select(season, match_round, player_team, 
           player_id, player_first_name, player_last_name, 
           player_height_cm, player_weight_kg, subbed) %>%
    filter(subbed %in% c('Subbed In', 'Subbed Out')) %>% 
    drop_na() %>% 
    group_by(season, match_round, player_team) %>% 
    summarise(
        n = n(),
        height_diff = max(player_height_cm) - min(player_height_cm),
        weight_diff = max(player_weight_kg) - min(player_weight_kg),
        names = paste(player_first_name, player_last_name, subbed, collapse = ', ')
    ) %>% 
    filter(n == 2) %>% 
    View()
    
# Pendlebury Height:191 cm Weight:90 kg
# Cox Height:211 cm Weight:105 kg
# diff 20cm 15kg

# Boak cap vs non-cap

afltables %>% 
    select(season, playing_for, jumper_no, id, first_name, surname, kicks, handballs, marks, goals) %>% 
    # filter(id %in% c(831, 11583, 12002, 1193, 303), playing_for == 'Port Adelaide') %>% 
    group_by(id, first_name, surname, jumper_no) %>% 
    summarise(
        n = n(),
        t_kicks = sum(kicks)/n,
        t_hb = sum(handballs)/n,
        t_marks = sum(marks)/n,
        t_goals = sum(goals)/n,
    ) %>% View()

#milestone matches

afltables %>% 
    select(season, round, id, first_name, surname, games_played) %>% 
    filter(games_played %in% c(1:10*50)) %>% 
    group_by(id, first_name, surname, round) %>% 
    summarise(
        n = n(),
        comb = paste(games_played, collapse = ", ")
    ) %>% View()

afltables %>% 
    select(season, round, games_played) %>% 
    filter(games_played %in% c(1:10*50)) %>% distinct() %>% 
    group_by(season, round) %>% 
    summarise(
        n = n(),
        comb = paste(games_played, collapse = ", ")
    ) %>% View()
