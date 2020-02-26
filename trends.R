
library(dplyr)
library(readr)
library(tidyr)

popularity_data <- readr::read_csv("multiTimeline.csv")

max_interest <- popularity_data %>% pivot_longer(cols = names(popularity_data)[2:19]) %>% 
  group_by(name) %>% 
  filter(value == max(value)) %>% 
  select('Team' = name, 'Max Interest' = value, Date) %>% 
  arrange(-`Max Interest`)

write.csv(max_interest, "max_interest.csv")
