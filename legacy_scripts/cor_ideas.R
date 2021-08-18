
# https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html
# https://www.flutterbys.com.au/stats/tut/tut10.6a.html

library(dplyr)
library(tidyr)
library(lubridate)
library(snakecase)
library(stringr)
library(fitzRoy)

source("load_afltables.R")

id_time_disp <- afltables %>% 
    filter(id == 11548) %>% 
    select(id, kicks, handballs)

plot(id_time_disp$kicks, id_time_disp$handballs)

afltables %>% 
    filter(season > 1965) %>% 
    group_by(id, first_name, surname) %>% 
    summarise(
        cor = cor(kicks, handballs),
        games = n(),
        t = sum(disposals)
    ) %>% 
    filter(games > 200) %>% 
    mutate(
        diff = abs(cor)
    ) %>% 
    arrange(diff)


afltables$time_on_ground

afltables_mod <-
    afltables %>% 
    filter(season > 2010) %>% 
    group_by(id, first_name, surname) %>% 
    mutate(games = n()) %>% ungroup() %>%
    filter(games > 50) %>% 
    select(local_start_time, disposals, id, first_name, surname) %>% 
    mutate(
        id = as.character(id)
    )

# dim(afltables_mod)
# 
# out_mod <- glm(disposals ~ (local_start_time + id)^2, data = afltables_mod,
#                family = poisson(link = 'log'))
# 
# summary(out_mod)
# par(mfrow = c(2,2))
# plot(out_mod)

library(tidyr)
library(broom)
library(purrr)

afltables_mod_out <- afltables_mod %>% 
    nest(-id,-first_name, -surname) %>% 
    mutate(
        fit = map(data, ~ glm(disposals ~ local_start_time, data = .x, family = poisson())),
        tidied = map(fit, tidy)
    ) %>% 
    unnest(tidied)

mod_min_mid_max <- 
    afltables_mod_out %>% 
    filter(term == 'local_start_time') %>% 
    filter(
        estimate == min(estimate) | estimate == max(estimate) | abs(estimate) == min(abs(estimate))
    ) %>% 
    mutate(est_hp2h = exp(estimate * 120))

mod_min_mid_max
mod_ids <- mod_min_mid_max %>% pull(id)

id_time_disp <- afltables_mod %>% 
    # filter(id == 12370 | id == 4178 | id == 11716) %>% #12422, 4088
    filter(id %in% mod_ids) %>% #12422, 4088
    mutate(name = paste(first_name, surname)) %>% 
    select(name, disposals, local_start_time)

library(ggplot2)

p <- ggplot(id_time_disp, aes(local_start_time, disposals, colour = name)) +
    xlab('Local Start Time (24hr)') +
    ylab('Disposals') + 
    ggtitle('Disposals by Start Time')

p + geom_point(alpha = 0.6) + 
    geom_smooth(method = 'glm', method.args = list(family = poisson())) #+ geom_jitter() #+ theme(legend.position = 'none')
