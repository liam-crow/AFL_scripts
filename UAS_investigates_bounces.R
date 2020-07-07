library(dplyr)

score_bounces <- afltables %>% 
    select(season, round, home_team, away_team, playing_for, playing_for_score, bounces) %>% 
    filter(season >= 1999) %>% 
    group_by(season, round, home_team, away_team, playing_for) %>% 
    summarise(
        playing_for_score = unique(playing_for_score),
        s_bounces = sum(bounces),
        .groups = 'drop'
    )

library(ggplot2)

p1 <- ggplot(score_bounces, aes(y = playing_for_score, x = s_bounces, colour = as.character(season))) + 
    geom_point(alpha = 0.25) +
    geom_smooth() +
    labs(
        x = "Total Team Bounces",
        y = "Team Score",
        colour = "Season"
    ) +
    ggtitle("Team Score vs Team Bounces by Year", "Useless AFL Stats") +
    theme(
        legend.position = 'bottom'
    ) +
    guides(col = guide_legend(ncol = 8))

p2 <- ggplot(score_bounces, aes(y = s_bounces, x = season)) + 
    geom_point(alpha = 0.15) +
    geom_smooth() +
    labs(
        x = "Season",
        y = "Team Total Bounces",
        colour = "Season"
    ) +
    ggtitle("Team Bounces by Year")

p3 <- ggplot(score_bounces, aes(y = playing_for_score, x = season)) + 
    geom_point(alpha = 0.15) +
    geom_smooth() +
    ylim(c(0,200)) +
    labs(
        x = "Season",
        y = "Team Score",
        colour = "Season"
    ) +
    ggtitle("Team Score by Year")

library(patchwork)

p1/(p2+p3)
