library(ggplot2)
library(extraDistr)
library(tibble)
# May need to install these packages 
# if running for the first time locally

# install.packages("ggplot2")
# install.packages("extraDistr")
# install.packages("tibble")

min_b <- 10000 # Lower Bound
max_b <- 45000 # Upper Bound
avg   <- 30000 # Chosen Average

# How many samples do you want to simulate
n_samples <- 100

# Choose scaling factor that is greater than 0
# Low values imply high variance and 
# High values imply low variance
a <- 0.1

# Run each line sequentially
m <- (avg-min_b)/(max_b-min_b)

b <- a/m-a

x <- extraDistr::rnsbeta(n_samples, a, b)

x_scaled <- x*(max_b-min_b)+min_b

# Consult graph to check distribution of data
ggplot(tibble::tibble(x = x_scaled), aes(x)) +
  # geom_density() +
  geom_density(color = "blue") + 
  geom_rug()
  
# Consult summary stats to check
mean(x_scaled)
sd(x_scaled)
max(x_scaled)
min(x_scaled)

write.csv(x_scaled, "data_generated.csv")
