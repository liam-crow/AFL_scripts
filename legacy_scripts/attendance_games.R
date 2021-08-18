
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- fitzRoy::get_afltables_stats(start_date = "1921-01-01")

afltables_filt <- afltables %>% select(ID, First.name, Surname, Attendance)

afltables_filt %>% 
  filter(Attendance <= 3000) %>% 
  group_by(ID, First.name, Surname) %>% 
  count() %>% 
  arrange(-n)

