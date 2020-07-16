
names_matches <- afltables %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        matches = n(),
        .groups = "drop"
    ) %>% 
    mutate(
        first_name = tolower(first_name),
        surname = tolower(surname)
    )

names_matches %>% 
    filter(
        grepl('[t]', surname) &
        grepl('[i]', surname) &
        grepl('[n]', surname) &
        grepl('[d]', surname) &
        grepl('[e]', surname) &
        grepl('[r]', surname)
    ) %>% arrange(-matches)

names_matches %>% 
    filter(
        grepl('[g]', surname) &
        grepl('[r]', surname) &
        grepl('[i]', surname) &
        grepl('[n]', surname) &
        grepl('[d]', surname) &
        grepl('[r]', surname) 
    ) %>% arrange(-matches)

names_matches %>% 
    filter(
        grepl('[h]', surname) &
        grepl('[i]', surname) &
        grepl('[n]', surname) &
        grepl('[g]', surname) &
        grepl('[e]', surname)
    ) %>% arrange(-matches)

names_matches %>% 
    filter(
        grepl('[b]', surname) &
        grepl('[u]', surname) &
        grepl('[m]', surname) &
        grepl('[b]', surname) &
        grepl('[l]', surname) &
        grepl('[e]', surname)
    ) %>% arrange(-matches)

names_matches %>% 
    filter(
        grepl('[e]', surname) &
        grepl('[h]', surname) &
        grepl('[a]', surname) &
        grepl('[r]', surname) &
        grepl('[m]', surname) &
        grepl('[o]', surname) &
        grepl('[n]', surname) &
        grepl('[y]', surname)
    ) %>% arrange(-matches)
