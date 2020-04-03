library(dplyr)
library(fitzRoy)

afltables_2019 <- fitzRoy::get_afltables_stats(start_date = "2019-01-01")

afltables_2019 %>% group_by(round = as.numeric(Round), Brownlow.Votes) %>% count() %>% arrange(round)


