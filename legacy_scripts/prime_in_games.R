
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(foreign)
library(ggplot2)
library(MASS)
library(lattice)

afltables <- fitzRoy::get_afltables_stats()

afltables %>% dplyr::select(Season, Round, Date, Home.team, Away.team ,starts_with("HQ"), starts_with("AQ"), Home.score, AQ4G, AQ4B, Away.score) %>% 
  distinct() %>% 
  mutate(
    HQ1T = HQ1G*6 + HQ1B,
    AQ1T = AQ1G*6 + AQ1B,
    HQ2T = HQ2G*6 + HQ2B,
    AQ2T = AQ2G*6 + AQ2B,
    HQ3T = HQ3G*6 + HQ3B,
    AQ3T = AQ3G*6 + AQ3B,
    HQ4T = HQ4G*6 + HQ4B,
    AQ4T = AQ4G*6 + AQ4B
  ) %>% 
  filter(
    # is_prime(HQ1B),
    is_prime(AQ1B),
    # is_prime(HQ2B),
    is_prime(AQ2B),
    # is_prime(HQ3B),
    is_prime(AQ3B),
    # is_prime(HQ4B),
    is_prime(AQ4B),
    # is_prime(HQ1G),
    is_prime(AQ1G),
    # is_prime(HQ2G),
    is_prime(AQ2G),
    # is_prime(HQ3G),
    is_prime(AQ3G),
    # is_prime(HQ4G),
    is_prime(AQ4G),
    # is_prime(HQ1T),
    is_prime(AQ1T),
    # is_prime(HQ2T),
    is_prime(AQ2T),
    # is_prime(HQ3T),
    is_prime(AQ3T),
    # is_prime(HQ4T),
    is_prime(AQ4T)
  ) %>% 
  arrange(desc(Date))

afl_stats <- afltables %>% dplyr::select(Brownlow.Votes, Kicks, Marks, Handballs, Goals, Clearances)

xyplot(Marks ~ Handballs | as.factor(Brownlow.Votes), afl_stats %>% distinct())

densityplot(~Marks, data = afl_stats, groups = Brownlow.Votes, auto.key = T, n = 10)

plotly::plot_ly(afl_stats %>% distinct(), x = ~Kicks, y = ~Clearances, z = ~Goals, 
                type = 'scatter3d', mode = 'markers',
                color = ~as.factor(Brownlow.Votes))

afl_stats %>% filter(Brownlow.Votes == 3)
is_prime(1953/7/9)
1953/7/9
