# devtools::install_github("jimmyday12/fitzRoy")
library(fitzRoy)
library(lattice)
library(dplyr)
library(lubridate)

results <- get_match_results()

results_wl <- results %>% 
  group_by(row_number()) %>% 
  mutate(win = max(Home.Points, Away.Points),
         loss = min(Home.Points, Away.Points)) %>% 
  ungroup()

unique_scores <-
  results_wl %>% 
  group_by(win, loss) %>% 
  count() %>% ungroup() %>% 
  filter(n == 1)

one_time_scores <- inner_join(results_wl, unique_scores, by = c("win","loss")) 

one_time_scores %>% filter(Date > "2019-01-01")

one_time_scores

