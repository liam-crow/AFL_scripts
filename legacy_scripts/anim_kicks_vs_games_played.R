
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- fitzRoy::get_afltables_stats(start_date = "1900-01-01")
names(afltables) <- snakecase::to_snake_case(names(afltables))

modern_afl_players <- 
    afltables %>% 
    select(season, round, date, playing_for, id, first_name, surname, kicks) %>%
    mutate(
        name = paste(substr(first_name,1,1), surname)
    ) %>% 
    group_by(id) %>% arrange(date) %>% 
    mutate(
        debut = min(date),
        sum_kicks = cumsum(kicks),
        games_played = row_number()
    ) %>% ungroup() %>% 
    filter(debut > '1965-01-01')

modern_afl_players

library(ggplot2)

p <- ggplot(modern_afl_players %>% filter(season %in% 1967))

# p + geom_point(aes(x = games_played, y = kicks))
p + geom_point(aes(x = games_played, y = sum_kicks, color = playing_for))

# p + geom_point(aes(x = date, y = kicks))
p + geom_point(aes(x = date, y = sum_kicks, color = playing_for))

library(gganimate)

anim <- ggplot(iris, aes(Petal.Width, Petal.Length, colour = Species)) +
    geom_point() +
    transition_filter(
        transition_length = 2,
        filter_length = 1,
        Setosa = Species == 'setosa',
        Long = Petal.Length > 4,
        Wide = Petal.Width > 2
    ) +
    ggtitle(
        'Filter: {closest_filter}',
        subtitle = '{closest_expression}'
    ) +
    enter_fade() +
    exit_fly(y_loc = 0)

anim2 <- ggplot(iris, aes(Petal.Width, Petal.Length, colour = Species)) +
    geom_point() +
    transition_filter(
        transition_length = 2,
        filter_length = 1,
        Setosa = Species == 'setosa',
        Long = Petal.Length > 4,
        Wide = Petal.Width > 2,
        keep = TRUE
    ) +
    ggtitle(
        'Filter: {closest_filter}',
        subtitle = '{closest_expression}'
    ) +
    exit_recolour(colour = 'grey') +
    exit_shrink(size = 0.5)
