
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- fitzRoy::get_afltables_stats(start_date = "1900-01-01")
names(afltables) <- snakecase::to_snake_case(names(afltables))

afltables %>% 
    group_by(id, first_name, surname) %>% count() %>% ungroup() %>% 
    filter(grepl('beard', surname, ignore.case = T))

afltables %>% 
    group_by(id, first_name, surname) %>% count() %>% ungroup() %>% 
    filter(grepl('pell', surname, ignore.case = T))



afltables %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        debut = min(date),
        sum_goals = sum(goals),
        sum_behinds = sum(behinds)
    ) %>% 
    mutate(
        sum_goals = if_else(is.na(sum_goals), 0, sum_goals),
        sum_behinds = if_else(is.na(sum_behinds), 0, sum_behinds),
        acc = sum_goals/(sum_goals + sum_behinds)
    ) %>% filter(debut > "1965-01-01", sum_goals > 10) %>% arrange(-acc) %>% View()


afltables %>% select(season, round, id, first_name, surname, hit_outs) %>% 
    group_by(season, round)

afltables %>% group_by(season, round, date, playing_for) %>% count(surname) %>% arrange(-n) %>% View()

afltables %>% group_by(season, playing_for) %>% 
    filter(date > '1965-01-01') %>% 
    summarise(
        goals = sum(goals),
        behinds = sum(behinds)
    ) %>% 
    mutate(points = goals *6 + behinds) %>% arrange(points) %>% View()

afltables %>% group_by(season, id, first_name, surname) %>% 
    filter(date > '1965-01-01') %>% 
    summarise(
        goals = sum(goals),
        behinds = sum(behinds)
    ) %>% arrange(-behinds) %>% 
    mutate(goals - behinds) %>% View()

afltables %>% select(first_name, surname) %>% 
    unite(name, first_name, surname, sep = '') %>% 
    mutate(name = tolower(name), char = nchar(name)) %>% distinct() %>% group_by(name) %>% 
    filter(grepl("^(?![a-z]*([a-z])[a-z]*\\1)[a-z]{1,30}$", name, perl = T)) %>% 
    arrange(-char)


afltables %>% group_by(id, first_name, surname) %>% 
    summarise(sum(goals), sum(kicks), sum(handballs), n()) %>% View()

afltables %>% group_by(jumper_no) %>% 
    summarise(disp = sum(kicks, na.rm = T) + sum(handballs, na.rm = T)) %>% 
    arrange(-disp)

Lockett_data <- afltables %>% filter(playing_for == 'St Kilda') %>% 
    group_by(season, round, date) %>% 
    filter(any(surname == 'Lockett')) %>% ungroup()

Lockett_data %>% filter(surname == 'Lockett') %>% 
    summarise(sum(goals))

Lockett_data %>% #filter(surname == 'Lockett') %>% 
    summarise(sum(goals))

afltables %>% group_by(season, round, home_team, away_team, umpire_1, umpire_2, umpire_3, umpire_4) %>%
    summarise(frees_for = sum(frees_for), frees_against = sum(frees_against)) %>% 
    pivot_longer(cols = starts_with('umpire'), names_to = 'id', values_to = 'umpire_name') %>%
    filter(umpire_name != "") %>% 
    group_by(umpire_name) %>% 
    summarise(
        ff = sum(frees_for), fa = sum(frees_against), n = n()
    ) %>% arrange(-ff)

afltables %>% filter(date > '1999-01-01', date < '2020-01-01') %>% group_by(season, playing_for) %>% 
    summarise(ins50 = sum(inside_50_s), m_ins50 = sum(marks_inside_50)) %>% 
    mutate(ratio = ins50/(ins50+m_ins50)) %>% arrange(-ratio)


library(mgsub)
lett_to_num <- afltables %>% select(id, first_name, surname) %>% 
    unite(name, first_name, surname, sep = '') %>% 
    mutate(
        name = tolower(name), 
        char = nchar(name), 
        name = gsub('[- ]','',name)
    ) %>% 
    distinct() %>% rowwise() %>% 
    mutate(
        conv = trimws(mgsub(string = name, pattern = letters, replacement = paste0(1:26, ' '))),
        prod = prod(as.numeric(unlist(strsplit(conv, ' ')))),
        add = sum(as.numeric(unlist(strsplit(conv, ' ')))),
        avg = mean(as.numeric(unlist(strsplit(conv, ' '))))
    )

afltables %>% group_by(id, first_name, surname) %>% summarise() %>% unite(name, first_name, surname, sep = ' ') %>% 
    filter(
        grepl('ff', name),
        grepl('n', name),
        grepl('u', name),
        grepl('y', name)
        )
    
afltables %>% 
    select(season, round, id, first_name, surname, playing_for, home_team, home_score, away_team, away_score) %>% 
    mutate(GF_win = 
               case_when(
                   round == 'GF' & playing_for == home_team & home_score > away_score ~ T,
                   round == 'GF' & playing_for == away_team & away_score > home_score ~ T,
                   TRUE ~ F
               )
    ) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(T = sum(GF_win)) %>% 
    filter(T == 0)


afltables %>% select(season, round, date, home_team, away_team, id, first_name, surname) %>% 
    filter(date > '1910-01-01') %>% 
    group_by(id) %>% mutate(debut = min(date)) %>% 
    group_by(season, round, home_team, away_team) %>% 
    summarise(
        dif = max(debut) - min(debut),
        mind = min(debut),
        maxd = max(debut)
    ) %>% 
    arrange(-dif) %>% View()

 <- afltables %>% 
    select(season, round, id, first_name, surname, playing_for, home_team, home_score, away_team, away_score) %>% 
    mutate(GF_win = 
               case_when(
                   round == 'GF' & playing_for == home_team & home_score > away_score ~ T,
                   round == 'GF' & playing_for == away_team & away_score > home_score ~ T,
                   TRUE ~ F
               )
    ) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        Total = sum(GF_win),
        n = n()
        ) %>% 
    filter(n >= 200)
