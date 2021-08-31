library(dplyr)
library(tidyr)
library(lubridate)
library(fitzRoy)
source("load_afltables.R")
source("fryzigg_data.R")

#### data prep ####
min_games <- 150

team_data <- afltables %>% 
    select(season, round, date, home_team, away_team, h_a, opp, playing_for, id, first_name, surname, w_l) %>% 
    filter(season >= 2000) %>% 
    mutate(
        season = as.character(season)
    ) %>% 
    group_by(id, first_name, surname) %>% 
    mutate(n = n()) %>% 
    filter(
        n >= min_games,
        w_l != 'D'
    ) %>% ungroup()


player_ids <- team_data %>% 
    select(id, first_name, surname) %>% 
    distinct() %>% 
    mutate(
        id = paste0('id',id),
        fullname = paste(first_name, surname)
    )

team_one_hot <- team_data %>% 
    pivot_wider(
        id_cols = c('season','round','date','playing_for','opp','h_a','w_l'),
        names_from = id,
        names_prefix = 'id',
        values_from = id,
        values_fill = list(id = F),
        values_fn = list(id = is.numeric)
    ) %>% 
    mutate(
        w_l_num = if_else(w_l == 'W',1,0), #switch to case when D
        .after = 'w_l'
    )

#### exploration ####

library(ggplot2)
library(patchwork)

p1 <- ggplot(team_one_hot, aes(x = w_l, colour = season, fill = season)) +
    geom_bar()
p2 <- ggplot(team_one_hot, aes(x = w_l)) +
    geom_bar()
p1/p2

# roughly normal
# good enough for me

#### ####

data_names <- names(team_one_hot)
id_names <- data_names[grepl('^id',data_names)]

# formula_1 <- paste('w_l_num ~ season +', paste(id_names, collapse = '+'))
formula_1 <- paste('w_l_num ~ ', paste(id_names, collapse = '+'))

model_1 <- glm(
    data = team_one_hot,
    formula = formula_1,
    family = binomial(link = 'logit')
)
summary(model_1)
par(mfrow=c(2,2))
plot(model_1)

formula_2 <- paste('w_l_num ~ season +', paste(id_names, collapse = '+'))

model_2 <- glm(
    data = team_one_hot,
    formula = formula_2,
    family = binomial(link = 'logit')
)

# model_2_aic <- MASS::stepAIC(model_2, direction = 'both')
summary(model_2)
par(mfrow=c(2,2))
plot(model_2)

library(broom)
#choose model
model_coef <- tidy(model_2)
model_confint <- model_coef %>% 
    mutate(
        estimate_exp = estimate,
        lb = estimate-1.96*std.error,
        ub = estimate+1.96*std.error,
        term = gsub('TRUE','',term)
    ) %>% arrange(estimate) %>% 
    slice(1:100)

model_confint_players <- model_confint %>% 
    inner_join(player_ids, by = c('term'='id'))


# %>% filter(grepl('^id',term))
ggplot(model_confint_players, aes(x=estimate_exp, y=reorder(fullname, desc(estimate)))) + 
    geom_errorbar(aes(xmin=lb, xmax=ub), width=.3) +
    geom_point() +
    ggtitle("The Effect of Port Adelaide Players on Final Score") + 
    xlab("Estimate (95% CI)") + ylab("Term")

ggplot(model_confint, aes(x=estimate_exp, y=reorder(term, desc(estimate)))) + 
    geom_errorbar(aes(xmin=lb, xmax=ub), width=.3) +
    geom_point() +
    ggtitle("The Effect of Port Adelaide Players on Final Score") + 
    xlab("Estimate (95% CI)") + ylab("Term")
