
# Loading Packages and Preprocessing ----

library(tidyverse)

pl <- read_csv("./Simulations/Data/premiereleague.csv", 
               col_names = c("home", "score1", "score2", "away"))

# Bayesian Hierarchical Model ----
# hyperpriors -> less susceptible to choice of hyperparameters in prior
