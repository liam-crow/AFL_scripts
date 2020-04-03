# devtools::install_github("jimmyday12/fitzRoy")
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

results <- get_match_results()

games_wl <- results %>% dplyr::select(Home.Points, Away.Points) %>% rowwise() %>% 
  dplyr::mutate(
    Win.Points = max(Home.Points, Away.Points),
    Loss.Points= min(Home.Points, Away.Points)
  ) %>% dplyr::select(Win.Points, Loss.Points)

comb_wl <- crossing(Home.Comb = 13:240, Away.Comb = 1:164) %>% rowwise() %>% 
  mutate(
    Win.Points = max(Home.Comb, Away.Comb),
    Loss.Points= min(Home.Comb, Away.Comb)
  ) %>% select(Win.Points, Loss.Points) %>% distinct()

comb_never_reached <- anti_join(comb_wl, games_wl, by = c("Win.Points", "Loss.Points"))

melted_games <- reshape2::melt(games_wl) %>% as.tbl() # replace with pivot_longer

score_count <- melted_games %>% group_by(value) %>% count() %>% arrange(-n)

comb_never_reached_win <- full_join(comb_never_reached, score_count, by = c('Win.Points' = 'value'))
comb_never_reached_all <- full_join(comb_never_reached_win, score_count, by = c('Loss.Points' = 'value'))

result <- comb_never_reached_all %>% 
  mutate(
    sum   = n.x + n.y,
    total = if_else(Win.Points == Loss.Points, sum/2, as.numeric(sum))
  ) %>% arrange(-total)

View(result)

lattice::densityplot(~total, result)

#           Win.Points  Loss.Points n.x   n.y   total
# 1         80          80          422   422   844
# 2         82          82          420   420   840
# 3         67          67          397   397   794
# 4         72          62          404   368   772
# 5         99          73          349   423   772
# 6         89          89          377   377   754
# 7         87          54          431   300   731
# 8         68          62          362   368   730
# 9         83          55          427   298   725
# 10        84          55          420   298   718
# ... with 19,481 more rows

#### Checking
# scores of 72 and 62 have been reached 403 and 365 times respectively AND 99 and 73 
# have appeared 348 and 421 times respectively, but the combinations have never 
# been reached
games_wl_test <- results %>% select(Home.Points, Away.Points) %>% rowwise() %>% 
  mutate(
    Win.Points = max(Home.Points, Away.Points),
    Loss.Points= min(Home.Points, Away.Points)
  ) %>% ungroup()

games_wl_test %>% filter(Win.Points == 99 | Loss.Points == 62)
