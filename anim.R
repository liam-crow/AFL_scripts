# devtools::install_github("jimmyday12/fitzRoy")

library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(ggplot2)

library(gifski)
library(png)
library(gganimate)

p <- ggplot(mtcars, aes(factor(cyl), mpg)) + 
    geom_boxplot() + 
    # Here comes the gganimate code
    transition_states(
        gear,
        transition_length = 2,
        state_length = 1
    ) +
    enter_fade() + 
    exit_shrink() +
    ease_aes('sine-in-out')

animate(p, nframes = 40, renderer = gifski_renderer("gganim.gif"))
