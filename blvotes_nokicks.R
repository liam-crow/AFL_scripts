# devtools::install_github("jimmyday12/fitzRoy")
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- get_afltables_stats()
names(afltables) <- names(afltables) %>% snakecase::to_snake_case()

afltables2019 <- afltables %>% filter(date > "1965-01-01")

afltables2019_brownlow <- afltables2019 %>% 
  filter(
    brownlow_votes > 0,
    marks == 0
  ) %>% 
  select(-starts_with('hq'), -starts_with('aq')) %>% View()

plotly::plot_ly(x = ~kicks, y = ~handballs, data = afltables2019,
                type = 'scatter,')
