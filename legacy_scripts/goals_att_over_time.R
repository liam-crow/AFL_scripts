
library(fitzRoy)
library(dplyr)
library(tidyr)
library(lubridate)

afltables <- fitzRoy::get_afltables_stats(start_date = "1960-01-01")
names(afltables) <- snakecase::to_snake_case(names(afltables))

atnd_acc <- afltables %>% 
    select(season, round, date, attendance, hq_4_g, hq_4_b, aq_4_g, aq_4_b) %>% 
    distinct() %>% 
    mutate(
        total_goals = hq_4_g + aq_4_g,
        total_behinds = hq_4_b + aq_4_g,
        total_score = total_goals*6 + total_behinds,
        acc = total_goals/(total_goals + total_behinds)*100
    )

library(patchwork)

library(ggplot2)

ggplot(atnd_acc, aes(x = attendance, y = acc)) + geom_point() + geom_smooth()
ggplot(atnd_acc, aes(x = total_goals, y = total_behinds)) + 
    geom_smooth() + geom_density2d()
fit <- lm(total_goals ~ total_behinds, data = atnd_acc)
summary(fit)

p1 <- ggplot(atnd_acc, aes(x = date, y = acc)) + geom_point() + geom_smooth()
p2 <- ggplot(atnd_acc, aes(x = date, y = total_goals)) + geom_point() + geom_smooth()
p3 <- ggplot(atnd_acc, aes(x = date, y = total_behinds)) + geom_point() + geom_smooth()
p4 <- ggplot(atnd_acc, aes(x = date, y = total_score)) + geom_point() + geom_smooth()

(p1+p2)/(p3+p4)

