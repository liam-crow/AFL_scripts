
library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)

source("load_afltables.R")

afl_id <- afltables %>% select(id, first_name, surname) %>% distinct()

# brownlow votes, frees for & against at 1984
# tackles at 1987
# rebounds, inside 50's, clearances, clangers at 1998
# cont & uncont pos, cont marks, marks inside 50, one perc, bounces at 1999
# goal assists at 2003

afltables_clean  <- 
  afltables %>% select(
    season, round, date, venue, local_start_time, id, playing_for, 
    home_team, home_g = hq_4_g, home_b = hq_4_b, home_score,
    away_team, away_g = aq_4_g, away_b = aq_4_b, away_score,
    kicks, marks, handballs, goals, behinds,
    hit_outs, tackles, rebounds, inside_50_s,
    clearances, clangers, frees_for, frees_against,
    contested_possessions, uncontested_possessions,
    contested_marks, marks_inside_50, one_percenters,
    bounces, goal_assists, brownlow_votes
  ) %>% filter(
    !(round %in% c('EF','QF','SF','PF','GF'))
  ) %>% mutate(
    home_away = if_else(playing_for == home_team, 'H', 'A'),
    brownlow_yn = if_else(brownlow_votes > 0, 1, 0),
    margin = if_else(playing_for == home_team, home_score - away_score, away_score - home_score),
    win_loss  = case_when(
      margin > 0 ~ 'W',
      margin < 0 ~ 'L',
      TRUE ~ 'D'
    ),
  ) %>% #select(-home_team, -away_team) %>% 
  group_by(id) %>% arrange(date) %>% 
  mutate(games_played = row_number()) %>% 
  ungroup()

# afltables_clean %>% select(playing_for, home_team, away_team, home_score, away_score, margin, home_away, win_loss) %>% distinct() %>% View()

afltables_group <- afltables_clean %>% group_by(date, venue, home_team, away_team) %>% 
  mutate(game_id = as.character(group_indices())) %>% ungroup()

afltables_mod  <- afltables_group %>% filter(between(date, as.Date('2017-01-01'), as.Date('2018-01-01')))
afltables_2018 <- afltables_group %>% filter(between(date, as.Date('2018-01-01'), as.Date('2019-01-01')))

library(ggplot2)
library(patchwork)

theme_set(theme_light())
p <- ggplot(afltables_mod)

p1 <- 
  p + geom_density(aes(x = kicks, group = as.character(brownlow_yn), fill = as.character(brownlow_yn)), alpha = 0.5) +
  ggtitle("Kicks by Brownlow Vote") + theme(legend.position = "right") + labs(fill = 'Browlow Y/N') + xlab('Kicks')

p2 <- 
  p + geom_density(aes(x = marks, group = as.character(brownlow_yn), fill = as.character(brownlow_yn)), alpha = 0.5, adjust = 2) +
  ggtitle("Marks by Brownlow Vote") + theme(legend.position = 'none') + xlab('Marks')

p3 <- 
  p + geom_density(aes(x = handballs, group = as.character(brownlow_yn), fill = as.character(brownlow_yn)), alpha = 0.5) +
  ggtitle("Handballs by Brownlow Vote") + theme(legend.position = 'none') + xlab('Handballs')

p4 <- 
  p + geom_density(aes(x = goals, group = as.character(brownlow_yn), fill = as.character(brownlow_yn)), alpha = 0.5, adjust = 3) +
  ggtitle("Goals by Brownlow Vote") + theme(legend.position = 'none') + xlab('Goals')


(p1 + p2) / (p3 + p4)

p5 <- 
  p + geom_density(aes(x = hit_outs, group = as.character(brownlow_yn), fill = as.character(brownlow_yn)), alpha = 0.5) +
  ggtitle("Hit Outs by Brownlow Vote") + theme(legend.position = "right") + labs(fill = 'Browlow Y/N') + xlab('Hit Outs')

p6 <- 
  p + geom_density(aes(x = tackles, group = as.character(brownlow_yn), fill = as.character(brownlow_yn)), alpha = 0.5, adjust = 2) +
  ggtitle("Tackles by Brownlow Vote") + theme(legend.position = 'none') + xlab('Tackles')

p7 <- 
  p + geom_density(aes(x = games_played, group = as.character(brownlow_yn), fill = as.character(brownlow_yn)), alpha = 0.5) +
  ggtitle("Games Played by Brownlow Vote") + theme(legend.position = 'none') + xlab('Games Played')

p8 <- 
  p + geom_density(aes(x = margin, group = as.character(brownlow_yn), fill = as.character(brownlow_yn)), alpha = 0.5) +
  ggtitle("Margin by Brownlow Vote") + theme(legend.position = 'none') + xlab('Margin')

(p5 + p6) / (p7 + p8)

View(afltables_mod)

library(brms)

#### null mod ####

null_f <- brmsformula(
  brownlow_yn ~ kicks + marks + handballs + goals + behinds + 
    hit_outs + tackles + rebounds + inside_50_s + 
    clearances + clangers + frees_for + frees_against +
    contested_possessions + uncontested_possessions +
    contested_marks + marks_inside_50 + one_percenters +
    bounces + goal_assists + 
    home_away + win_loss + games_played + margin) #+ (1|game_id))

null_p <- get_prior(
  formula = null_f,
  data = afltables_mod,
  family = 'bernoulli'
)

null_mod <- brm(
  null_f,
  data = afltables_mod,
  family = 'bernoulli',
  prior = null_p,
  thin = 1,
  inits = 0,
  chains = 4,
  cores  = 4,
  iter = 1000,
  control = list(max_treedepth = 8, adapt_delta = 0.7)
)

plot(null_mod, ask = F)

pp_check(null_mod) + theme_bw()

plot(conditional_effects(null_mod))

#### new mod ####

mod1_f <- brmsformula(
  brownlow_yn ~ kicks + marks + handballs + goals + 
    hit_outs + tackles + 
    clearances + frees_for +
    contested_possessions + uncontested_possessions +
    contested_marks + one_percenters +
    bounces + margin) #+ (1|game_id))

mod1_p <- get_prior(
  formula = mod1_f,
  data = afltables_mod,
  family = 'bernoulli'
)

mod1_mod <- brm(
  mod1_f,
  data = afltables_mod,
  family = 'bernoulli',
  prior = mod1_p,
  thin = 2,
  inits = 0,
  chains = 4,
  cores  = 4,
  iter = 1000,
  control = list(max_treedepth = 8, adapt_delta = 0.7)
)

mcmc_plot(mod1_mod)
mcmc_plot(mod1_mod, type = 'dens')
mcmc_plot(mod1_mod, type = 'dens_overlay')
mcmc_plot(mod1_mod, type = 'violin')
bayesplot::mcmc_hex(mod1_mod,pars = c('b_handballs', 'b_kicks'))

mcmc_plot(mod1_mod, type = 'neff')
mcmc_plot(mod1_mod, type = 'rhat')

mcmc_plot(mod1_mod, type = 'nuts_acceptance')
mcmc_plot(mod1_mod, type = 'nuts_divergence')
mcmc_plot(mod1_mod, type = 'nuts_stepsize')
mcmc_plot(mod1_mod, type = 'nuts_treedepth')
mcmc_plot(mod1_mod, type = 'nuts_energy')

pp_check(mod1_mod) + theme_bw()

plot(conditional_effects(mod1_mod))

#### interaction mod ####

mod2_f <- brmsformula(
  brownlow_yn ~ margin * (kicks + handballs + marks + goals) + 
    hit_outs + tackles + 
    clearances + frees_for +
    contested_possessions + uncontested_possessions +
    contested_marks + one_percenters +
    bounces) #+ (1|game_id))

mod2_p <- get_prior(
  formula = mod2_f,
  data = afltables_mod,
  family = 'bernoulli'
)

mod2_mod <- brm(
  mod2_f,
  data = afltables_mod,
  family = 'bernoulli',
  prior = mod2_p,
  thin = 2,
  inits = 0,
  chains = 4,
  cores  = 4,
  iter = 1000,
  control = list(max_treedepth = 8, adapt_delta = 0.7)
)

plot(mod2_mod, ask = F)

pp_check(mod2_mod) + theme_bw()

plot(conditional_effects(mod2_mod))


#### model comparison ####

loo(null_mod, mod1_mod, mod2_mod)

votes_2018 <- tibble(
  votes = afltables_2018$brownlow_votes,
  votes_yn = afltables_2018$brownlow_yn
)

predictions <- predict(null_mod, newdata = afltables_2018, re_formula = "brownlow_yn ~ (1|game_id)", allow_new_levels = T)
predictions <- predict(null_mod, newdata = afltables_2018)

afltables_2018$pred <- predictions[,1]

# View(afltables)
# plot(votes_2019$votes, votes_2019$pred)

ggplot(afltables_2018, aes(x = as.factor(brownlow_votes), y = pred)) +
  geom_point() +
  geom_boxplot()

library(pROC)

mod_roc <- roc(afltables_2018$brownlow_yn, afltables_2018$pred)
auc(mod_roc)
