
library(rvest)
library(dplyr)
library(tidyr)
library(mgsub)
library(stringr)

cv_url <- "https://aflcoaches.com.au/awards/the-aflca-champion-player-of-the-year-award/leaderboard/2021/202101"
result_table <- NULL

for (i in 1:23) {
    round <- str_pad(i,2,'left','0')
    cv_url_round <- paste0(cv_url,round)
    coaches_votes_url <- read_html(cv_url_round)
    
    cv_numbers <- coaches_votes_url %>% 
        html_elements('.col-2') %>% 
        html_text() %>% 
        mgsub("\n|\t",'')
    
    cv_players <- coaches_votes_url %>% 
        html_elements('.col-10') %>% 
        html_text() %>% 
        mgsub::mgsub("\n|\t",'')
    
    result_table_round <- tibble(
        votes = cv_numbers,
        players = cv_players,
        round = as.character(i)
    )
    result_table <- rbind(result_table, result_table_round)
}

coaches_votes_data <- result_table %>% 
    mutate(
        players = gsub("'",'',players),
        players = case_when(
            players == 'Samuel Walsh (CARL)' ~ 'Sam Walsh (CARL)',
            players == 'Josh J Kennedy (WCE)' ~ 'Josh Kennedy (WCE)',
            players == 'Tom J Lynch (RICH)' ~ 'Tom Lynch (RICH)',
            players == 'Harry Himmelberg (GWS)' ~ 'Harrison Himmelberg (GWS)',
            players == 'Jordan De Goey (COLL)' ~ 'Jordan deGoey (COLL)',
            players == 'Mitchell Georgiades (PORT)' ~ 'Mitch Georgiades (PORT)',
            players == 'Paddy Ryder (STK)' ~ 'Patrick Ryder (STK)',
            players == 'Harrison McKay (CARL)' ~ 'Harry McKay (CARL)',
            players == 'Brendon Ah Chee (WCE)' ~ 'Brendon AhChee (WCE)',
            players == 'Matt Taberner (FRE)' ~ 'Matthew Taberner (FRE)',
            T ~ players
        )
    ) %>% 
    separate(players,c('first_name','surname','playing_for_short'), sep = ' ', remove = F) %>% 
    mutate(playing_for_short = gsub('\\(|\\)','',playing_for_short)) %>% 
    filter(votes != 'Votes')

source("load_afltables.R")

afltables_data <- afltables %>% 
    select(season, date, round, playing_for_short, opp_short, h_a, id, first_name, surname, w_l) %>% 
    filter(season == 2021)

joined_data <- inner_join(
        coaches_votes_data, afltables_data, 
        by = c('round','playing_for_short','first_name','surname')
    ) %>% 
        mutate(votes = as.numeric(votes))

# total votes per game
# (5+4+3+2+1)*2 = 30

cv_ratio <- joined_data %>% 
    group_by(season, round, playing_for_short, w_l) %>% 
    summarise(
        total_votes = sum(votes)
    ) %>% 
    group_by(playing_for_short, w_l) %>% 
    summarise(
        share_of_votes = round(mean(total_votes)/30*100,2)
    )

library(ggplot2)

team_colours <- c(
    'CARL'= '#0e1e2d',
    'GEEL'= '#1c3c63',
    'ADEL'= '#002b5c',
    'SYD' = '#ed171f',
    'STK' = '#ed0f05',
    'ESS' = '#cc2031',
    'RICH'= '#D8D800',
    'MELB'= '#0f1131',
    'HAW' = '#4d2004',
    'BL'  = '#a30046',
    'GCFC'= '#d93e39',
    'FRE' = '#2a1a54',
    'COLL'= '#000000',
    'PORT'= '#01b5b6',
    'WB'  = '#014896',
    'WCE' = '#062ee2',
    'GWS' = '#f15c22',
    'NMFC'= '#013b9f'
)

factor_levels <- cv_ratio %>% filter(w_l == 'W') %>% arrange(-share_of_votes) %>% pull(playing_for_short)

cv_ratio_ordered <- transform(cv_ratio, playing_for_short = factor(playing_for_short,levels=factor_levels))

ggplot(cv_ratio_ordered, aes(x = playing_for_short, y = share_of_votes, fill = playing_for_short)) +
    geom_bar(stat = 'identity') +
    scale_fill_manual(values = team_colours) +
    xlab('Team')+
    ylab('Average % share of the votes')+
    theme(
        legend.position = 'none'
    ) +
    facet_wrap(~w_l, nrow = 3) +
    geom_text(aes(label = round(share_of_votes)), nudge_y = 10) +
    ggtitle(label = 'Average Percentage Share of Coaches Votes by Result', subtitle = "@crow_data_sci")

    
    
