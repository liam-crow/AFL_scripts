source("load_afltables.R")
library(dplyr)
library(tidyr)
library(stringr)
library(httr)

venue_data <- afltables %>% 
    filter(season >= 2018) %>%
    distinct(venue) %>% 
    mutate(
        venue_tidy = case_when(
            venue == "Victoria Park" ~ "Victoria Park Melbourne",
            venue == "Waverley Park" ~ "Waverley Park Melbourne",
            venue == "Olympic Park" ~ "Olympic Boulevard Melbourne",
            venue == "Corio Oval" ~ "Corio Oval Geelong",
            venue == "Football Park" ~ "Turner Drive, West Lakes",
            venue == "York Park" ~ "University of Tasmania stadium",
            venue == "Western Oval" ~ "Whitten Oval",
            venue == "Punt Rd" ~ "Punt Rd Oval",
            venue == "Brunswick St" ~ "Brunswick St Oval",
            venue == "Arden St" ~ "Arden Street Oval",
            venue == "Blacktown" ~ "Blacktown AFL/Cricket Stadium",
            venue == "Carrara" ~ "Carrara Stadium",
            venue == "Gabba" ~ "The Gabba",
            venue == "Subiaco" ~ "Subiaco Oval",
            venue == "North Hobart" ~ "North Hobart Oval",
            TRUE ~ venue
        ),
        venue_tidy = str_replace_all(venue_tidy,'\\.','')
    ) %>% 
    filter(!(venue_tidy %in% c('Jiangwan Stadium','Wellington'))) %>% 
    select(venue, venue_tidy) 

#

#### testing ####
url <- "https://places.nbnco.net.au/places/v1/autocomplete"

response <- GET(
    url, 
    add_headers(referer = 'https://www.nbnco.com.au/learn/rollout-map'), 
    query = list(query = 'Glenferrie Oval', timestamp = as.integer(Sys.time()))
)

content_get <- content(response)
resp_content <- unlist(content_get$suggestions[1])



# url_loc <- "https://places.nbnco.net.au/places/v2/location/ChIJjTecptlD1moRq-m5hk7gKkQ?source=website_rollout_map"
url_loc <- "https://places.nbnco.net.au/places/v2/location/"
response_loc  <- GET(
    paste0(url_loc, resp_content[1]), 
    add_headers(referer = 'https://www.nbnco.com.au/learn/rollout-map'), 
    query = list(source = "website_rollout_map")
)
content_get_loc <- content(response_loc)

#### for loop implementation ####

content_loc <- NULL
url <- "https://places.nbnco.net.au/places/v1/autocomplete"
url_loc <- "https://places.nbnco.net.au/places/v2/location/"

for (i in 1:length(venue_data$venue_tidy)) {
    response <- GET(
        url, 
        add_headers(referer = 'https://www.nbnco.com.au/learn/rollout-map'), 
        query = list(query = venue_data$venue_tidy[i], timestamp = as.integer(Sys.time()))
    )
    
    content_get <- content(response)
    resp_content<- unlist(content_get$suggestions[1])
    
    response_loc  <- GET(
        paste0(url_loc, resp_content[1]), 
        add_headers(referer = 'https://www.nbnco.com.au/learn/rollout-map'), 
        query = list(source = "website_rollout_map")
    )
    
    if(response_loc$status_code == 200){
        content_get_loc <- as.data.frame(content(response_loc))
        content_get_loc$venue <- venue_data$venue[i]
        content_loc <- rbind(content_loc, content_get_loc)
    } else {
        print(i)
        print(venue_data$venue[i])
    }
}

afl_venue_locations <- as_tibble(content_loc) %>% 
    mutate(id = row_number())

# write.csv(afl_venue_locations, "afl_venue_locations.csv", row.names = F)

# plotly::plot_ly(
#     afl_venue_locations, x = ~longitude, y = ~latitude, 
#     type = 'scatter', mode = 'markers',
#     hoverinfo = 'text', text = ~venue
# )

library(leaflet)

leaflet(data = afl_venue_locations) %>% addTiles() %>%
    addMarkers(~longitude, ~latitude, popup = ~venue, label = ~venue)

library(geosphere)

distance <- as.matrix(distm(select(afl_venue_locations, longitude, latitude), fun = distHaversine))/1000

dist_fun <- function(i, j) {
    vapply(seq_along(i), function(k) distance[i[k], j[k]], numeric(1L))
}

(n <- length(afl_venue_locations$id))


library(ompr)
model <- MILPModel() %>%
    # we create a variable that is 1 iff we travel from city i to j
    add_variable(x[i, j], i = 1:n, j = 1:n, 
                 type = "integer", lb = 0, ub = 1) %>%
    
    # a helper variable for the MTZ formulation of the tsp
    add_variable(u[i], i = 1:n, lb = 1, ub = n) %>% 
    
    # minimize travel distance
    set_objective(sum_expr(colwise(dist_fun(i, j)) * x[i, j], i = 1:n, j = 1:n), "max") %>%
    
    # you cannot go to the same city
    set_bounds(x[i, i], ub = 0, i = 1:n) %>%
    
    # leave each city
    add_constraint(sum_expr(x[i, j], j = 1:n) == 1, i = 1:n) %>%
    #
    # visit each city
    add_constraint(sum_expr(x[i, j], i = 1:n) == 1, j = 1:n) %>%
    
    # ensure no subtours (arc constraints)
    add_constraint(u[i] >= 2, i = 2:n) %>% 
    add_constraint(u[i] - u[j] + 1 <= (n - 1) * (1 - x[i, j]), i = 2:n, j = 2:n)
model


library(ompr.roi)
library(ROI.plugin.glpk)

result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

(result_val <- round(objective_value(result), 2))

solution <- get_solution(result, x[i, j]) %>% 
    filter(value > 0)

paths <- select(solution, i, j) %>% 
    rename(from = i, to = j) %>% 
    mutate(trip_id = row_number()) %>% 
    # pivot_longer(
    #     cols = c('to', 'from'),
    #     names_to = 'location',
    #     values_to = 'idx_val'
    # ) %>% 
    # mutate(idx_val = as.integer(idx_val)) %>% 
    inner_join(afl_venue_locations, by = c("from" = "id"))

# library(ggplot2)
# ggplot(afl_venue_locations, aes(longitude, latitude)) + 
#     geom_point() + 
#     geom_line(data = paths, aes(group = trip_id)) + 
#     ggtitle(paste0("Optimal route with cost: ", result_val))

paths_leaflet <- paths[1,]
paths_row <- paths[1,]

library(leaflet)
for (i in 1:17) {
    paths_row <- paths %>% filter(from == paths_row$to[1])
    
    paths_leaflet <- rbind(paths_leaflet, paths_row)
}

leaflet() %>% 
    addTiles() %>%
    addMarkers(data = afl_venue_locations, ~longitude, ~latitude, popup = ~venue, label = ~venue) %>% 
    addPolylines(data = paths_leaflet, ~longitude, ~latitude, weight = 2)

paste0(paths_leaflet$venue, collapse = ' -> ')

s <- seq(0,100,0.1)
length(s)

data <- rnorm(1001, s,s)
plot(data)
