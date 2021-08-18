devtools::install_github("jimmyday12/fitzRoy")
library(fitzRoy)
library(lattice)
library(dplyr)

results <- get_match_results()

results_old <- results %>% filter(Date < "2019-01-01")

results_new <- results %>% filter(Date > "2019-01-01")


xyplot(Winner ~ Loser, total_agg_comb, groups = type,
       par.settings = list(superpose.symbol = list(pch = 19, cex = c(0.5, 0.1),
                                                   col = c("red", "gray"))))

anti_join(total_agg_new, total_agg_old, by = c("Winner", "Loser")) %>% 
  filter(type == "new")

densityplot(~Margin, results)
