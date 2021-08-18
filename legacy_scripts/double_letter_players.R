afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname) %>% 
    filter(
        grepl("ll", surname, ignore.case = T)
    ) %>% 
    group_by(season, round, date, playing_for) %>% 
    count() %>% arrange(-n, -season)

afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname) %>% 
    filter(
        season == 1971, round == '17', playing_for == 'Carlton'
    ) %>% 
    filter(
        grepl("ll", surname, ignore.case = T)
    )

afltables %>% 
    select(season, round, date, local_start_time, home_team, away_team) %>% distinct() %>% 
    # filter(weekdays(date) == 'Sunday') %>% 
    arrange(-local_start_time)
