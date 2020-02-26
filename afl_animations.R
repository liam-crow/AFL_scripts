# devtools::install_github("jimmyday12/fitzRoy")
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- get_afltables_stats()
names(afltables) <- names(afltables) %>% snakecase::to_snake_case()

afltables_modern <- afltables %>% 
  filter(
    date > "2019-01-01",
    !(round %in% c('EF','QF','SF','PF','GF'))
  ) %>% 
  mutate(
    round = as.numeric(round)
  )

library(ggplot2)
library(gganimate)
theme_set(theme_bw())

p <- 
  ggplot(
    afltables_modern,
    aes(x = round, y = marks, colour = playing_for)
  ) + 
  geom_line(show.legend = F, alpha = 0.7)

p + transition_reveal(round)

