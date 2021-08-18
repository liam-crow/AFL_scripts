library(raster)
library(ggplot2)
library(dplyr)
pulsar <- function(n){
    x <- seq(0,100, length=n)
    norms <- dnorm(x, mean=40, sd=6)*100 + dnorm(x, mean=60, sd=6)*50
    noise1 <- approx(sample(c(rep(0:8,2),18:19)), n = n, y=NULL)$y
    noise2 <- approx(sample(0:50), n = n, y=NULL)$y
    noise3 <- rnorm(n)
    abs(norms + norms * noise1 + norms * noise2 * .1 + noise3)
}

d <- sapply(rep(100,50), pulsar)
dim(d)
d <- data.frame(rasterToPoints(raster(d)))
dim(d)
d$elev <- d$layer + d$y * 1200

p0 <- ggplot() + 
    labs(x = 'UNKNOWN PLEASURES', y = NULL, title = 'JOY DIVISION') + 
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          plot.background = element_rect(fill = "black"),
          plot.title = element_text(colour = 'white', size = 76, hjust = .5),
          axis.title.x = element_text(colour = 'white', size = 43))

for (g in unique(d$y)) {
    dat <- subset(d, y == g)
    p0 <- p0 + geom_polygon(data=dat, aes(x, elev), fill='black') + 
        geom_line(data=dat, aes(x, elev), col='white', size=.5)
    p0
}

# ggsave(filename = 'unknown_pleasures.png', plot = p0, w=8, h = 8)
# browseURL('unknown_pleasures.png')

team_colour_scheme <- readr::read_csv("joy_division/team_colour_scheme.csv")

for (i in 1:nrow(team_colour_scheme)) {
    
    team <- team_colour_scheme$team[i]
    nickname <- team_colour_scheme$nickname[i]
    letter_c <- team_colour_scheme$letter_c[i]
    bg_c <- team_colour_scheme$bg_c[i]
    font_size <- team_colour_scheme$font_size[i]
    
    rows <- 100
    fill_col <- 20
    
    JD_data <- afltables %>% 
        filter(playing_for == team) %>% 
        dplyr::select(
            home_team, away_team, date,
            pq_1_g, pq_1_b, pq_2_g, pq_2_b, pq_3_g, pq_3_b, pq_4_g, pq_4_b,
            oq_4_g, oq_4_b, oq_3_g, oq_3_b, oq_2_g, oq_2_b, oq_1_g, oq_1_b
        ) %>% distinct() %>% arrange(desc(date)) %>% 
        select_if(is.numeric) %>% 
        slice(1:rows)
    
    JD_data <- as.matrix(JD_data)
    JD_data <- cbind(matrix(rnorm(fill_col*rows, sd = 0.3), rows, fill_col), JD_data)
    JD_data <- cbind(JD_data, matrix(rnorm(fill_col*rows, sd = 0.3), rows, fill_col))
    
    JD_data <- data.frame(rasterToPoints(raster(JD_data)))
    
    JD_data$elev <- JD_data$layer + JD_data$y * 400
    
    p <- ggplot() + 
        labs(x = 'UNKNOWN PLEASURES', y = NULL, title = paste0(nickname,' DIVISION'), subtitle = 'Useless AFL Stats') + 
        theme_minimal() +
        theme(axis.ticks = element_blank(),
              panel.grid = element_blank(),
              axis.text = element_blank(),
              plot.background = element_rect(fill = bg_c),
              plot.title = element_text(colour = letter_c, size = font_size, hjust = .5, vjust = -2),
              plot.subtitle = element_text(colour = letter_c, size = 10, hjust = .5, vjust = 30),
              axis.title.x = element_text(colour = letter_c, size = 43))
    
    for (g in unique(JD_data$y)) {
        dat <- subset(JD_data, y == g)
        p <- p + geom_polygon(data = dat, aes(x, elev), fill = bg_c) + 
            geom_line(data = dat, aes(x, elev), col = letter_c, size = .5)
    }
    
    ggsave(filename = paste0('joy_division/',team ,'_unknown_pleasures.png'), plot = p, w = 8, h = 10, dpi = 500)
}
