afltables %>% 
    group_by(id, first_name, surname, playing_for) %>% 
    summarise(
        n = n(),
        debut = min(date)
    ) %>% ungroup() %>% 
    mutate(
        full_name = tolower(paste0(first_name, surname, sep = ''))
    ) %>% 
    filter(
        grepl('[b]', full_name),
        grepl('[a]', full_name),
        grepl('[r]', full_name),
        grepl('[s]', full_name),
        playing_for == 'Port Adelaide'
    )

afltables %>% 
    group_by(id, first_name, surname, playing_for) %>% 
    summarise(
        n = n(),
        debut = min(date)
    ) %>% ungroup() %>% 
    mutate(
        full_name = tolower(paste0(first_name, surname, sep = ''))
    ) %>% 
    filter(
        grepl('[p]', full_name),
        grepl('[r]', full_name),
        grepl('[i]', full_name),
        grepl('[s]', full_name),
        grepl('[o]', full_name),
        grepl('[n]', full_name),
        playing_for == 'Adelaide'
    )
