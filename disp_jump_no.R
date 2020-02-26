# devtools::install_github("jimmyday12/fitzRoy")
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- get_afltables_stats()
names(afltables) <- names(afltables) %>% snakecase::to_snake_case()

disp_jump <- afltables %>% select(date, round, id, first_name, surname, jumper_no, kicks, handballs) %>% 
  filter(
    date > "1965-01-01",
    jumper_no == kicks + handballs
  ) #%>% 
disp_jump %>% filter(id == 1680) %>% View()

disp_jump %>% group_by(id, first_name, surname) %>% 
  summarise(n = n(), date = max(date)) %>% arrange(-n) %>% 
  filter(date > "2015-01-01") %>% View()
