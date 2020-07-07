library(fitzRoy)
library(dplyr)

footywire_stats <- as_tibble(fitzRoy::update_footywire_stats())
names(footywire_stats) <- snakecase::to_snake_case(names(footywire_stats))


