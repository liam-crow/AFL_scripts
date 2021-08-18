tips <- fitzRoy::get_squiggle_data("tips") %>% as_tibble()

View(tips)

tips_red <- tips %>% select(year, round, gameid, hteam, ateam, source, tip, confidence, correct) 
  # arrange(year, -round, gameid) %>% 
  # group_by(gameid)

tips_red %>% filter(year == 2018) %>% select()

avg_conf_upsets <- tips_red %>% group_by(gameid) %>% 
  filter(correct == 0) %>% 
  summarise(
    n = n(),
    avg_conf = mean(confidence),
    max_conf = max(confidence),
    min_conf = min(confidence),
    year = unique(year),
    round = unique(round),
    hteam = unique(hteam),
    ateam = unique(ateam),
    tip = unique(tip)
  ) %>% 
  arrange(-n,-avg_conf)

write.csv(avg_conf_upsets, "avg_conf_upsets.csv")





