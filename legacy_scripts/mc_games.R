afltables %>% 
    select(season, round, date, home_team, away_team, hq_2_g, hq_2_b, aq_2_g, aq_2_b, hq_4_b, aq_4_b) %>% distinct() %>% 
    mutate(
        total_points = hq_4_b + aq_4_b
    ) %>% arrange(total_points)

library(lubridate)
gb_dob <- afltables %>% 
    select(date, id, playing_for, first_name, surname, goals, behinds) %>% 
    filter(
        month(date) == behinds & day(date) == goals |
        day(date) == behinds & month(date) == goals
    )

write.csv(gb_dob,'gb_dob.csv')

afltables %>% 
    select(season, round, home_team, away_team, playing_for, id, first_name, surname, goals) %>% 
    filter(grepl('^mc.', surname, ignore.case = T)) %>% distinct() %>% 
    # filter(goals > 0) %>% 
    group_by(season, round, home_team, away_team) %>% 
    count() %>% arrange(-n)

afltables %>% 
    select(season, round, home_team, away_team, playing_for, id, first_name, surname, behinds) %>% 
    filter(behinds >= 5) %>% distinct() %>% 
    group_by(season, round, home_team, away_team, playing_for) %>% 
    count() %>% ungroup() %>% #arrange(-n)
    group_by(n) %>% count()

afltables %>% 
    select(season, round, home_team, away_team, playing_for, id, first_name, surname, behinds) %>% 
    filter(behinds %in% c(5,6)) %>% distinct() %>% 
    group_by(season, round, home_team, away_team, playing_for) %>% 
    summarise(
        unq = length(unique(behinds)),
        n = n()
    ) %>% 
    filter(unq == 2, n == 2) %>% arrange(-season)
