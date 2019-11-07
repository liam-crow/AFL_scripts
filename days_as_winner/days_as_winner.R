# devtools::install_github("jimmyday12/fitzRoy")
library(fitzRoy)
library(dplyr)
library(lubridate)
library(zoo)

results <- get_match_results() %>% filter(Date > "2019-01-01")

data <- results %>% select(Date, Home.Team, Away.Team, Margin)

teams <- unique(data$Home.Team)
date_vec <- tibble(
  Date = seq.Date(as.Date("2019-01-01"),as.Date("2019-12-31"), by = "day")#,
  # Win = NA_integer_
)

agg <- NULL

for (i in 1:18){
  team <- teams[i]
  team_games <- data %>% filter(Home.Team == team | Away.Team == team) %>% 
    mutate(Mult = if_else(Away.Team == team, -1, 1)) %>% 
    mutate(Win.Margin = Margin*Mult) %>% 
    mutate(Win_YN = if_else(Win.Margin > 0, 1 , 0)) %>% 
    select(Date, Win_YN)
  days_WL <- left_join(date_vec, team_games, by = "Date")
  days_WL$Win_YN[1] <- 0
  
  days_WL$Win_all <- zoo::na.locf(days_WL$Win_YN)
  days_as_winner  <- sum(days_WL$Win_all)
  
  team_days_as_winner <- tibble(team = team, days_as_winner = days_as_winner)
  agg <- rbind(agg, team_days_as_winner)
}

agg %>% arrange(-days_as_winner)

write.csv(agg_sorted, "days_as_winner.csv", row.names = F)