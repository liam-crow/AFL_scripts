
gamesdata <- afltables %>% 
    select(season, round, date, home_team, away_team, playing_for, id, first_name, surname) %>% 
    group_by(id) %>% arrange(date) %>% 
    mutate(game_no = row_number()) %>% ungroup() %>% 
    mutate(
        initials = tolower(paste0(strtrim(first_name, 1),strtrim(surname, 1)))
    ) %>% 
    filter(game_no %in% c(1:12*50))

gamesdata %>% 
    group_by(season, round, date, home_team, away_team, game_no, initials) %>% 
    summarise(
        n = n()
    ) %>% 
    arrange(-n,-season)

View(gamesdata)
