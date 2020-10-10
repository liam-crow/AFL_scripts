
library(stringr)
play_by_play <- fitzRoy::get_score_progression_raw()
play_by_play_game <- play_by_play[1:55,]
names(play_by_play_game) <- c("home_team", "home_team_time", "score", "away_team_time","away_team")

play_by_play_game %>% 
    mutate(quarter_total_time = str_extract(home_team, "^(.+?)\\)")) %>% View()
    # filter(!grepl('^(1st|2nd|3rd|Final) quarter', home_team)) %>% 
    separate(score, into = c('home_team_score', 'away_team_score'), sep = ' - ') %>% View()

shopping_list <- c("(apples) x4 ", "bag of flour", "bag of sugar", "milk x2")
gsub(".*( (.+) ).*", "", shopping_list)
str_extract(play_by_play_game[2,1], "^(.+?)\\)")
str_extract(shopping_list, "[a-z]+")
str_extract(shopping_list, "[a-z]{1,4}")
str_extract(shopping_list, "\\b[a-z]{1,4}\\b")

# Extract all matches
str_extract_all(shopping_list, "[a-z]+")
str_extract_all(shopping_list, "\\b[a-z]+\\b")
str_extract_all(shopping_list, "\\d")

# Simplify results into character matrix
str_extract_all(shopping_list, "\\b[a-z]+\\b", simplify = TRUE)
str_extract_all(shopping_list, "\\d", simplify = TRUE)

# Extract all words
str_extract_all("This is, suprisingly, a sentence.", boundary("word"))
