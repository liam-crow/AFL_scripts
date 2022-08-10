
library(rvest)
library(dplyr)
library(tidyr)
library(mgsub)
library(stringr)

cv_url <- "https://aflcoaches.com.au/awards/the-aflca-champion-player-of-the-year-award/leaderboard/"
result_table <- NULL

# for (k in 2007:2021) {
for (k in 2021) {
    
    cv_url_season <- paste0("https://aflcoaches.com.au/awards/the-aflca-champion-player-of-the-year-award/leaderboard/",k,"/",k,"01")
    
    for (i in 1:25) {
        skip<-F
        round <- str_pad(i,2,'left','0')
        cv_url_round <- paste0(cv_url_season,round)
        
        #If page doesn't exist go to next round/year
        tryCatch(read_html(cv_url_round), error = function(e) {skip<<-T})
        if(skip) {next} 
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
            round = as.character(i),
            season = k
        )
        result_table <- rbind(result_table, result_table_round)
        print(i)
    }
    print(k)
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
            players == 'Tom J Lynch (GCFC)' ~ 'Tom Lynch (GCFC)',
            players == 'Jeffrey White (MELB)' ~ 'Jeff White (MELB)',
            players == 'Daniel Jacobs (HAW)' ~ 'Danny Jacobs (HAW)',
            players == 'Paddy Ryder (ESS)' ~ 'Patrick Ryder (ESS)',
            players == 'Paddy Ryder (PORT)' ~ 'Patrick Ryder (PORT)',
            players == 'Bradley Johnson (WB)' ~ 'Brad Johnson (WB)',
            players == 'Nicholas Stevens (CARL)' ~ 'Nick Stevens (CARL)',
            players == 'Gregory Tivendale (RICH)' ~ 'Greg Tivendale (RICH)',
            players == 'James Bartel (GEEL)' ~ 'Jimmy Bartel (GEEL)',
            players == 'Nicholas Fosdike (SYD)' ~ 'Nic Fosdike (SYD)',
            players == 'Dom Cassisi (PORT)' ~ 'Domenic Cassisi (PORT)',
            players == 'Nathan Van Berlo (ADEL)' ~ 'Nathan vanBerlo (ADEL)',
            players == 'Timothy Notting (BL)' ~ 'Tim Notting (BL)',
            players == 'Desmond Headland (FRE)' ~ 'Des Headland (FRE)',
            players == 'Nick Dal Santo (STK)' ~ 'Nick DalSanto (STK)',
            players == 'Nick Dal Santo (NMFC)' ~ 'Nick DalSanto (NMFC)',
            players == 'Christopher Bryan (COLL)' ~ 'Chris Bryan (COLL)',
            players == 'Christopher Tarrant (FRE)' ~ 'Chris Tarrant (FRE)',
            players == 'Matthew Suckling (HAW)' ~ 'Matt Suckling (HAW)',
            players == 'Mark Lecras (WCE)' ~ 'Mark LeCras (WCE)',
            players == 'Jamie MacMillan (NMFC)' ~ 'Jamie Macmillan (NMFC)',
            players == 'Alexander Keath (ADEL)' ~ 'Alex Keath (ADEL)',
            players == 'Nicholas Gill (ADEL)' ~ 'Nick Gill (ADEL)',
            players == 'Thomas Swift (WCE)' ~ 'Tom Swift (WCE)',
            players == 'Bradley Ottens (GEEL)' ~ 'Brad Ottens (GEEL)',
            players == 'Aaron VandenBerg (MELB)' ~ 'Aaron Vandenberg (MELB)',
            players == 'Timothy Boyle (HAW)' ~ 'Tim Boyle (HAW)',
            players == 'Malcolm Michael (ESS)' ~ 'Mal Michael (ESS)',
            players == 'Bradley Fisher (CARL)' ~ 'Brad Fisher (CARL)',
            players == 'Jeffrey Farmer (FRE)' ~ 'Jeff Farmer (FRE)',
            players == 'Joel MacDonald (BL)' ~ 'Joel Macdonald (BL)',
            players == 'Clinton Bizzell (MELB)' ~ 'Clint Bizzell (MELB)',
            players == 'Joshua Carr (FRE)' ~ 'Josh Carr (FRE)',
            players == 'William Thursfield (RICH)' ~ 'Will Thursfield (RICH)',
            players == 'Edward Sansbury (NMFC)' ~ 'Eddie Sansbury (NMFC)',
            players == 'Samuel Power (WB)' ~ 'Sam Power (WB)',
            players == 'Samuel Power (NMFC)' ~ 'Sam Power (NMFC)',
            players == 'Matt De Boer (FRE)' ~ 'Matthew deBoer (FRE)',
            players == 'Matt De Boer (GWS)' ~ 'Matthew deBoer (GWS)',
            players == 'Thomas Logan (PORT)' ~ 'Tom Logan (PORT)',
            players == 'Nicholas Duigan (CARL)' ~ 'Nick Duigan (CARL)',
            players == 'Matthew Maguire (BL)' ~ 'Matt Maguire (BL)',
            players == 'Callum Ah Chee (BL)' ~ 'Callum AhChee (BL)',
            players == 'Callum Ah Chee (GCFC)' ~ 'Callum AhChee (GCFC)',
            players == 'Mitch W Brown (MELB)' ~ 'Mitch Brown (MELB)',
            players == 'Callum L Brown (COLL)' ~ 'Callum Brown (COLL)',
            players == 'David MacKay (ADEL)' ~ 'David Mackay (ADEL)',
            players == 'Cameron Ellis-Yolmen (ADEL)' ~ 'Cam Ellis-Yolmen (ADEL)',
            players == 'Matthew Suckling (WB)' ~ 'Matt Suckling (WB)',
            players == 'Cameron McCarthy (FRE)' ~ 'Cam McCarthy (FRE)',
            players == 'Cameron McCarthy (GWS)' ~ 'Cam McCarthy (GWS)',
            players == 'Matthew Scharenberg (COLL)' ~ 'Matt Scharenberg (COLL)',
            players == 'Matthew White (PORT)' ~ 'Matt White (PORT)',
            players == 'Matthew White (RICH)' ~ 'Matt White (RICH)',
            players == 'Thomas Boyd (WB)' ~ 'Tom Boyd (WB)',
            players == 'Tommy Sheridan (FRE)' ~ 'Tom Sheridan (FRE)',
            players == 'Aaron vandenBerg (MELB)' ~ 'Aaron Vandenberg (MELB)',
            players == 'Brendon Ah Chee (PORT)' ~ 'Brendon AhChee (PORT)',
            players == 'Michael Pyke (SYD)' ~ 'Mike Pyke (SYD)',
            players == 'Brent MacAffer (COLL)' ~ 'Brent Macaffer (COLL)',
            players == 'Matthew Thomas (RICH)' ~ 'Matt Thomas (RICH)',
            players == 'Matthew Thomas (PORT)' ~ 'Matt Thomas (PORT)',
            players == 'Daniel Stanley (GCFC)' ~ 'Danny Stanley (GCFC)',
            players == 'Bradley Dalziell (WCE)' ~ 'Bradd Dalziell (WCE)',
            players == 'Bradley Dalziell (BL)' ~ 'Bradd Dalziell (BL)',
            players == 'Mitchell Brown (WCE)' ~ 'Mitch Brown (WCE)',
            players == 'Alexander Johnson (SYD)' ~ 'Alex Johnson (SYD)',
            players == 'Clinton Bartram (MELB)' ~ 'Clint Bartram (MELB)',
            players == 'Samuel Sheldon (BL)' ~ 'Sam Sheldon (BL)',
            players == 'Christopher Tarrant (COLL)' ~ 'Chris Tarrant (COLL)',
            players == 'Matthew Campbell (NMFC)' ~ 'Matt Campbell (NMFC)',
            players == 'Edward Barlow (WB)' ~ 'Ed Barlow (WB)',
            players == 'Samuel Iles (GCFC)' ~ 'Sam Iles (GCFC)',
            players == 'Bradley Dick (COLL)' ~ 'Brad Dick (COLL)',
            players == 'Mitchell Hahn (WB)' ~ 'Mitch Hahn (WB)',
            players == 'Timothy Houlihan (WCE)' ~ 'Tim Houlihan (WCE)',
            players == 'Thomas Williams (WB)' ~ 'Tom Williams (WB)',
            players == 'Matthew Jones (MELB)' ~ 'Matt Jones (MELB)',
            players == 'Sebastian Tape (GCFC)' ~ 'Seb Tape (GCFC)',
            players == 'Bradley Moran (ADEL)' ~ 'Brad Moran (ADEL)',
            players == 'Bradley Symes (ADEL)' ~ 'Brad Symes (ADEL)',
            players == 'Jarrod Moore (SYD)' ~ 'Jarred Moore (SYD)',
            players == 'Christopher Johnson (MELB)' ~ 'Chris Johnson (MELB)',
            players == 'Timothy Clarke (HAW)' ~ 'Tim Clarke (HAW)',
            T ~ players
        )
    ) %>% 
    separate(players,c('first_name','surname','playing_for_short'), sep = ' ', remove = F) %>% 
    mutate(playing_for_short = gsub('\\(|\\)','',playing_for_short)) %>% 
    filter(votes != 'Votes')

# source("load_afltables.R")

afltables_data <- afltables %>% 
    select(season, date, round, playing_for_short, opp_short, h_a, id, first_name, surname, w_l, brownlow_votes) %>% 
    filter(season %in% 2007:2021)

joined_data <- right_join(
    coaches_votes_data, afltables_data, 
    by = c('season','round','playing_for_short','first_name','surname')
) %>% 
    mutate(votes = as.numeric(votes)) %>% 
    replace_na(list(votes = 0)) %>% 
    filter(round %in% c(1:25))

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

brownlow_exploration <- joined_data %>% filter(season != 2021, !(votes == 0 & brownlow_votes == 0))
ggplot(brownlow_exploration, aes(x = votes)) +
    geom_bar() +
    facet_grid(season~brownlow_votes) +
    xlab('Coaches Votes') +
    ylab('Count') +
    ggtitle('Coaches Votes by Brownlow Medal Votes')

joined_data %>% 
    filter(brownlow_votes == 3, votes == 10) %>% 
    group_by(id, first_name, surname) %>% count() %>% arrange(-n)
