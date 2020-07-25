source("load_afltables.R")

no_unique <- afltables %>% 
    select(season, round, home_team, away_team, home_score, away_score) %>% distinct() %>% 
    filter(round %in% 1:25) %>% 
    group_by(season, round) %>% 
    summarise(
        games = n(),
        all_scores = paste0(home_score, away_score, collapse = ''),
        .groups = 'keep'
    ) %>% 
    mutate(
        sorted_numbers = paste(sort(unlist(strsplit(all_scores,''))), collapse = ''),
        unique_numbers = paste(sort(unique(unlist(strsplit(all_scores,'')))), collapse = ''),
        no_unique_numbers = nchar(unique_numbers)
    ) %>% 
    arrange(no_unique_numbers) #%>% 
    select(-all_scores)

View(no_unique)
write.csv(no_unique, "unique_final_score_numbers_per_round.csv", row.names = F)

no_unique %>% group_by(no_unique_numbers) %>% count()
