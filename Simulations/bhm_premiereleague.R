
# Loading Packages and Preprocessing ----
library(tidyverse)
library(rstan)

# rstan presets
numcores <- parallel::detectCores()
options(mc.cores = numcores - 4)


pl <- read_csv("./Simulations/Data/premiereleague.csv", 
               col_names = c("home", "score1", "score2", "away"))

teams <- unique(pl$home)
n_teams <- length(teams)
n_games_total <- nrow(pl)
n_predict <- 5
n_games <- n_games_total - n_predict
home_teams <- sapply(1:n_games_total, \(x) which(teams == pl$home[x]))
away_teams <- sapply(1:n_games_total, \(x) which(teams == pl$away[x]))

# Bayesian Hierarchical Model ----
# hyperpriors -> less susceptible to choice of hyperparameters in prior

model_script <- "./Simulations/bhm_premiereleague.stan"
bhm_model <- stan_model(model_script)



bhm.post <- sampling(bhm_model,
                           data = list(
                             n_teams = n_teams,
                             n_games = n_games,
                             home = home_teams[1:n_games],
                             away = away_teams[1:n_games],
                             score1 = pl$score1[1:n_games],
                             score2 = pl$score2[1:n_games],
                             n_predict = 5,
                             home_new = home_teams[(n_games + 1) : n_games_total],
                             away_new = away_teams[(n_games + 1) : n_games_total]
                           ),
                           chains = numcores-4,
                           iter = 10000,
                           warmup = 3000,
                           thin = 2,
                           show_messages = FALSE,
                           save_warmup = F,
                           seed = 42)

# extract parameters
params <- rstan::extract(bhm.post)

pred_scores <- c(colMeans(params$score1new), colMeans(params$score2new))
true_scores <- c(pl$score1[(n_games + 1) : n_games_total],
                 pl$score2[(n_games + 1) : n_games_total])


plot(true_scores, pred_scores, xlim=c(0,5), ylim=c(0,5), pch=20, ylab='predicted scores', xlab='true scores')
abline(a=0,  b=1, lty='dashed')

pred_errors = c(sapply(1:n_predict, function(x) sd(params$score1new[,x])),sapply(1:n_predict, function(x) sd(params$score2new[,x])))
arrows(true_scores, pred_scores+pred_errors, true_scores, pred_scores-pred_errors, length = 0.05, angle = 90, code = 3, col=rgb(0,0,0,0.3))


