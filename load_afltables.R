library(fitzRoy)
library(dplyr)

afltables <- fitzRoy::get_afltables_stats(start_date = "1905-01-01")
names(afltables) <- to_snake_case(names(afltables))

afltables <- afltables %>%
    mutate(
        id = case_when(
            id == 0 ~ 12592,
            TRUE ~ id
        ),
        first_name = case_when(
            id == "12592" ~ "Cam",
            TRUE ~ first_name
        ),
        surname = case_when(
            id == "12592" ~ "Rayner",
            TRUE ~ surname
        ),
        playing_for = case_when(
            id == "12592" ~ "Brisbane Lions",
            TRUE ~ playing_for
        )
    )