
library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
source("load_afltables.R")
source("fryzigg_data.R")

res <- afltables %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        debut = min(date),
        games = n(),
        teams = paste0(unique(playing_for_short), collapse = ', '),
        .groups = 'keep'
    ) %>% 
    mutate(
        surname = tolower(surname)
    ) %>% ungroup() %>% 
    mutate(
        brew_name = gsub('cas', 'case', surname),
        brew_name = gsub('drew', 'brew', brew_name),
        brew_name = gsub('bew', 'brew', brew_name),
        brew_name = gsub('^tan', 'tin', brew_name),
        brew_name = gsub('^ab', 'slab', brew_name),
        brew_name = gsub('^an', 'can', brew_name),
        brew_name = gsub('^.an', 'can', brew_name),
        brew_name = gsub('^in', 'tin', brew_name),
        brew_name = gsub('^.in', 'tin', brew_name),
        brew_name = gsub('^ev', 'bev', brew_name),
        brew_name = gsub('^.ev', 'bev', brew_name),
        # brew_name = snakecase::to_title_case(brew_name)
    ) %>% 
    filter(surname != brew_name, games > 49, debut > '1964-01-01') %>% 
    arrange(-games) %>% 
    mutate(comb = paste0(first_name,' ',snakecase::to_title_case(brew_name),' ',games,' (',teams,')'))

res %>% pull(comb) %>% paste(collapse = '\n')

afltables %>% 
    select(id, first_name, surname) %>% distinct() %>% 
    group_by(first_name) %>% count() %>% View()

afltables %>% 
    select(id, first_name, surname, date, round, w_l) %>% 
    group_by(id, first_name, surname) %>% 
    mutate(
        season = year(date),
        debut = min(season),
        retir = max(season)
    ) %>% 
    filter(round == 'GF', w_l == 'L', season == debut | season == retir) %>% 
    group_by(id, first_name, surname, debut, retir) %>% 
    count() %>% View()

afltables %>% 
    group_by(playing_for_short, id, first_name, surname) %>% 
    summarise(
        n = n(),
        min_d = min(date),
        .groups = 'keep'
    ) %>% arrange(min_d)

afltables %>% 
    select(date, season, round, playing_for, opp, playing_for_score, opp_score, w_l) %>% 
    distinct() %>% 
    mutate(
        total = playing_for_score + opp_score,
        p_pcnt = playing_for_score/total*100,
        o_pcnt = opp_score/total*100,
        diff = p_pcnt - o_pcnt
    ) %>% arrange(diff) %>% 
    filter(
        between(diff,-0.5,0.5),
        # diff != 0
    ) %>% View()
    group_by(playing_for, w_l) %>% count() %>% 
    pivot_wider(names_from = w_l, values_from = n, values_fill = 0) %>% 
    mutate(total = W+D+L) %>% View()

afltables %>% 
    select(id, first_name, surname) %>% 
    group_by(id, first_name, surname) %>% 
    count() %>% 
    mutate(
        l_first = length(unlist(strsplit(first_name,''))),
        l_surn = length(unlist(strsplit(surname,''))),
        ratio = l_first/l_surn,
        g_ratio = n/l_surn
    ) %>% View()

# dick_data <- fryzigg_data %>% 
#     select(player_id, player_first_name, player_last_name, player_height_cm, player_weight_kg) %>% 
#     distinct() %>% 
#     filter(grepl('^rich', player_first_name, ignore.case = T) | grepl('^rich', player_last_name, ignore.case = T))
# 
# dick_data_2 <- fryzigg_data %>% 
#     select(player_id, player_first_name, player_last_name, player_height_cm, player_weight_kg) %>% 
#     distinct() %>% 
#     filter(
#         grepl('^dick', player_first_name, ignore.case = T) | 
#         grepl('^dick', player_last_name, ignore.case = T)
#     )
# 
# write.csv(dick_data_2, file = "dick_data_2.csv", row.names = F)
# 
# dick_data_comb <- read.csv('dick_data.csv', header = T) %>% as_tibble()
# dick_data_comb %>% View()

afl_alpha <- afltables %>% 
    group_by(id, first_name, surname) %>% summarise(debut = min(date),n = n()) %>% 
    rowwise() %>% 
    mutate(
        fullname_raw = paste0(first_name, surname) %>% tolower() %>% trimws(which = 'both'),
        fullname = gsub('[^\\w]','',fullname_raw, perl = T),
        alphabetical = paste0(sort(unlist(strsplit(fullname,''))), collapse = '')
    ) %>% 
    select(-fullname_raw, -fullname)

pokedex <- read.csv("pokedex.csv") %>% as_tibble() %>% select(pokedex_number, name) %>% 
    rowwise() %>% 
    mutate(
        fullname_raw = name %>% tolower() %>% trimws(which = 'both'),
        fullname = gsub('[^\\w]','',fullname_raw, perl = T),
        alphabetical = paste0(sort(unlist(strsplit(fullname,''))), collapse = '')
    ) %>% 
    select(-fullname_raw, -fullname)

inner_join(afl_alpha, pokedex, by = 'alphabetical') %>% View()

paste0(sort(unlist(strsplit('ilovebeer',''))), collapse = '')
View(afl_alpha)

aflmargin <- afltables %>% 
    select(date, id, first_name, surname, playing_for_score, opp_score) %>% 
    mutate(
        debut = min(date),
        margin = playing_for_score - opp_score,
        Fullname = paste(first_name, surname)
    )

data_mar <- aflmargin %>% 
    group_by(id, first_name, surname) %>% 
    mutate(games = n()) %>% 
    filter(games >= 50) %>% 
    summarise(sd = sd(margin)) %>% View()

library(ggplot2)

ggplot(filter(aflmargin, id %in% c(2140,1938)), aes(x = margin, fill = Fullname, color = Fullname)) + 
    geom_density(alpha = 0.5) + 
    geom_rug() +
    xlab("Margin") +
    ylab("Density") +
    ggtitle("Player Margin Density Plot") +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())

afltables %>%
    filter(round == 'GF' & w_l == 'W') %>% 
    group_by(surname) %>% 
    count() %>% 
    arrange(-n)

betting_odds <- get_footywire_betting_odds(start_season = "2010") %>% as_tibble()
names(betting_odds) <- snakecase::to_snake_case(names(betting_odds))

View(betting_odds)
betting_odds %>% 
    filter((home_margin > 0 & home_margin < 6) | (away_margin > 0 & away_margin < 6)) %>% 
    select(season, round, home_team, away_team, home_score, away_score, home_win_odds, away_win_odds, home_win_paid, away_win_paid) %>% View()

betting_odds %>% 
    filter(home_score == home_win_odds*100 | away_score == away_win_odds*100) %>% 
    select(season, round, home_team, away_team, home_score, away_score, home_win_odds, away_win_odds) %>% 
    mutate(paste0(season,' r',round,' ',home_team,' vs ',away_team,' ',home_score,' - ',away_score,' ($',home_win_odds,' - $',away_win_odds,')')) %>% View()

betting_odds %>% 
    filter(home_score == (home_win_odds-1)*100 | away_score == (away_win_odds-1)*100) %>% 
    select(season, round, home_team, away_team, home_score, away_score, home_win_odds, away_win_odds)

afltables %>% 
    filter(date > "1965-01-01") %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        first_game_zero = date == min(date) & disposals == 0,
        n = n()
    ) %>% distinct() %>% View()

afltables %>% 
    filter(date > "1965-01-01") %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        m_date = min(date),
        first_game_zero = date == min(date) & disposals >= 22,
        n = n()
    ) %>% distinct() %>% 
    filter(n <= 22) %>% 
    View()
