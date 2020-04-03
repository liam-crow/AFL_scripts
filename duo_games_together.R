# devtools::install_github("jimmyday12/fitzRoy")

library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)

afltables <- fitzRoy::get_afltables_stats()

names(afltables) <- to_snake_case(names(afltables))

player_id <- afltables %>% select(id, first_name, surname) %>% distinct()

generate_comb <- function(id){
    id1 <- id
    id2 <- id
    comb_ord <- crossing(id1, id2) %>% 
        filter(id1 != id2) %>% 
        transmute(
            id_sml = if_else(id1 < id2, id1, id2),
            id_big = if_else(id1 > id2, id1, id2)
        ) %>% distinct()
    return(as_tibble(comb_ord))
}

player_games <-
    afltables %>% #filter(season == 2019) %>% 
    select(season, round, playing_for, id) %>% 
    group_by(season, round, playing_for) %>% 
    nest() 

n <- nrow(player_games)
comb_all <- NULL

library(foreach)
library(doParallel)

cl <- makeCluster(8)
registerDoParallel(cl)

t <- Sys.time()
ls <- foreach(i = seq(1,n), .combine = rbind, .inorder = F, .packages = c('dplyr','tidyr')) %dopar% {
    ids     <- player_games$data[[i]]$id
    comb_id <- generate_comb(ids)
    # print(i)
    # print(comb_id)
    
    comb_all_next <- data_frame(
        season = player_games$season[i],
        round  = player_games$round[i],
        playing_for = player_games$playing_for[i],
        id_sml = comb_id$id_sml,
        id_big = comb_id$id_big
    )
    
    return(comb_all_next)
}
Sys.time()-t

comb_agg <- ls %>% group_by(id_sml, id_big) %>% count() %>% arrange(-n)

comb_join <- comb_agg %>% ungroup() %>% inner_join(player_id, by = c('id_sml' = 'id')) %>% 
    inner_join(player_id, by = c('id_big' = 'id')) %>% 
    select(
        id1 = id_sml, fn1 = first_name.x, sn1 = surname.x,
        id2 = id_big, fn2 = first_name.y, sn1 = surname.y, games = n
    )

write.csv(comb_join, 'duo_games_together.csv')
