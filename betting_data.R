# devtools::install_github("jimmyday12/fitzRoy")

library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)

fw_stats <- get_footywire_stats(9700)

?get_footywire_stats

betting_odds_2019 <- get_footywire_betting_odds(start_season = "2019") %>% as_tibble()

betting_odds_2019 <- betting_odds_2019 %>% 
    mutate(
        home_win_odds_inv = 1/Home.Win.Odds,
        away_win_odds_inv = 1/Away.Win.Odds,
        Favourite = if_else(Home.Win.Odds < Away.Win.Odds, "Home", "Away")
    )

library(ggplot2)

p_inv <- ggplot(betting_odds_2019, aes(x = home_win_odds_inv, y = away_win_odds_inv, color = Favourite)) + 
    geom_point() + 
    geom_smooth(method = 'lm', se = T) + 
    ggtitle("Inverse Home vs Away Odds") +
    xlab("Inverse Home Winning Odds") +
    ylab("Inverse Away Winning Odds")

p_untrans <- ggplot(betting_odds_2019, aes(x = Home.Win.Odds, y = Away.Win.Odds, color = Favourite)) + 
    geom_point() + 
    geom_smooth(method = 'loess', se = F) +
    ggtitle("Home vs Away Odds") +
    xlab("Home Winning Odds") +
    ylab("Away Winning Odds")
    
library(png)
logo <- readPNG("useless_afl_logo.png")
p_inv + annotation_raster(logo, ymin = 0.75, ymax = 1, xmin = 0.75, xmax = 1.02)

mod <- lm(home_win_odds_inv ~ away_win_odds_inv, data = betting_odds_2019)

par(mfrow=c(2,2))
summary(mod)
plot(mod)

par(mfrow=c(1,1))
plot(home_win_odds_inv ~ away_win_odds_inv, data = betting_odds_2019)
abline(mod)


