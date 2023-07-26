
# Package Loading and Preprocessing ---------------------------------------
library(rstan)
library(parallel)
library(coda) # for gelman plot
library(hexbin)
library(tidyverse)
library(cowplot)

ts <- data.frame(year <- time(EuStockMarkets))
stocks <- data.frame(EuStockMarkets)

stocks.plrets <- sapply(stocks, 2, function(x) 100 * diff(log(x), 1))
y <- stocks.plrets[, 4] # FTSE
dim(stocks)
dim(stocks.plrets)


plot(ts[-length(ts),], y, type = "h")
abline(h = 0, col = "lightgrey")



numcores <- parallel::detectCores()
options(mc.cores = numcores - 4)


# ARCH Parameter Simulation -----------------------------------------------
arch_script <- "./BayesianAnalysis/arch.stan"
arch_model <- stan_model(arch_script)



arch.post <- sampling(arch_model,
                     data = list(T = length(y), r = y), 
                     chains = numcores-4,
                     iter = 10000,
                     warmup = 1000,
                     thin = 5,
                     show_messages = FALSE,
                     control = list(
                       adapt_delta = .99,
                       max_treedepth = 20
                     ),
                     save_warmup = F,
                     seed = 42)
# traceplot(arch.post)
summary(arch.post)

gelman.plot(As.mcmc.list(arch.post))

par(mfrow = c(2,2))
for (i in 1:4){
  acf(acf(as.matrix(arch.post), plot = F, lag = 1000)$acf[, i, i], lag = 1000, 
      main = c("mu", "alpha0", "alpha1", "lp___")[i])
}

bayesplot::mcmc_areas_ridges(arch.post, c("mu", "alpha0", "alpha1"))

bayesplot::mcmc_acf(arch.post)

bayesplot::color_scheme_set("red")
bayesplot::mcmc_intervals(arch.post, pars = c("mu", "alpha0", "alpha1"))

bayesplot::mcmc_hist(arch.post, pars = c("mu", "alpha0", "alpha1"))
bayesplot::color_scheme_set("teal")
bayesplot::mcmc_dens_overlay(arch.post)  
bayesplot::color_scheme_set("pink")
bayesplot::mcmc_violin(arch.post)

bayesplot::color_scheme_set("viridis")
bayesplot::mcmc_pairs(arch.post, pars = c("mu", "alpha0", "alpha1"),
                      diag_fun = "hist", off_diag_fun = "hex")

bayesplot::color_scheme_set("viridis")
bayesplot::mcmc_hex(arch.post, pars = c("mu", "alpha0"))


arch.params <- rstan::extract(arch.post)

mu <- mean(arch.params$mu)
alpha0 <- mean(arch.params$mu)
alpha1 <- mean(arch.params$alpha1)

# predicted volatility
pred.arch <- sapply(2:1860, function(x) mu + sqrt(alpha0 + alpha1 * (y[x-1] - mu)^2))

# GARCH Parameter Simulation -----------------------------------------------
garch_script <- "./BayesianAnalysis/garch.stan"
garch_model <- stan_model(garch_script)



garch.post <- sampling(garch_model,
                      data = list(T = length(y), r = y, sigma1 = 0.1), 
                      chains = numcores-4,
                      iter = 10000,
                      warmup = 1000,
                      thin = 5,
                      show_messages = FALSE,
                      control = list(
                        adapt_delta = .99,
                        max_treedepth = 20
                      ),
                      save_warmup = F,
                      seed = 42)
# traceplot(garch.post)
summary(garch.post)

# gelman.plot(As.mcmc.list(garch.post))
# 
# par(mfrow = c(2,2))
# for (i in 1:4){
#   acf(acf(as.matrix(garch.post), plot = F, lag = 1000)$acf[, i, i], lag = 1000, 
#       main = c("mu", "alpha0", "alpha1", "lp___")[i])
# }

bayesplot::color_scheme_set("brightblue")
bayesplot::mcmc_areas_ridges(garch.post, c("mu", "alpha0", "alpha1", "beta1"))

bayesplot::mcmc_acf(garch.post, c("mu", "alpha0", "alpha1", "beta1"))

bayesplot::color_scheme_set("red")
bayesplot::mcmc_intervals(garch.post, pars = c("mu", "alpha0", "alpha1", "beta1"))

bayesplot::mcmc_hist(garch.post, pars = c("mu", "alpha0", "alpha1", "beta1"))
bayesplot::color_scheme_set("teal")
bayesplot::mcmc_dens_overlay(garch.post)  
bayesplot::color_scheme_set("pink")
bayesplot::mcmc_violin(garch.post)

bayesplot::color_scheme_set("viridis")
bayesplot::mcmc_pairs(garch.post, pars = c("mu", "alpha0", "alpha1", "beta1"),
                      diag_fun = "hist", off_diag_fun = "hex")

bayesplot::color_scheme_set("viridis")
bayesplot::mcmc_hex(garch.post, pars = c("mu", "alpha0"))


garch.params <- rstan::extract(garch.post)

mu <- mean(garch.params$mu)
alpha0 <- mean(garch.params$mu)
alpha1 <- mean(garch.params$alpha1)
beta1 <- mean(garch.params$beta1)
sigma <- colMeans(garch.params$sigma)


# predicted volatility
pred.garch <- sapply(2:1860, function(x) mu + sqrt(alpha0 + alpha1 * (y[x-1] - mu)^2 + beta1 * sigma[x-1]^2))
uncertainty.garch <- sapply(2:1860, function(x) quantile(mu + sqrt(alpha0 + alpha1 * (y[x-1] - mu)^2 + beta1 * sigma[x-1]^2)), c(.025, .975))


# Plotting ARCH vs GARCH --------------------------------------------------
arch.df <- data.frame(time = ts[-length(ts),], plrets = y, pred = pred.arch, neg_pred = -pred.arch)
arch.plot <- arch.df %>% 
  ggplot() +
  geom_line(aes(x = time, y = plrets, color = "plrets")) + 
  geom_line(aes(x = time, y = pred, color = "pred")) +
  geom_line(aes(x = time, y = neg_pred, color = "pred")) +
  geom_hline(aes(yintercept = 0), alpha = .5, color = "lightgrey") +
  scale_colour_manual(values = c("black", "red"),
                      labels = c("percentage log returns", "ARCH volatility predictions")) +
  # theme_bw() +
  guides(color = guide_legend(title = "")) +
  labs(x = "Time", y = "Percentage Log Returns",
       title = "ARCH Predictions")

garch.df <- data.frame(time = ts[-length(ts),], plrets = y, pred = pred.garch, neg_pred = -pred.garch)

garch.plot <- garch.df %>% 
  ggplot() +
  geom_line(aes(x = time, y = plrets, color = "plrets")) + 
  geom_line(aes(x = time, y = pred, color = "pred")) +
  geom_line(aes(x = time, y = neg_pred, color = "pred")) +
  geom_hline(aes(yintercept = 0), alpha = .5, color = "lightgrey") +
  scale_colour_manual(values = c("black", "red"),
                      labels = c("percentage log returns", "GARCH volatility predictions")) +
  theme_bw() +
  guides(color = guide_legend(title = "")) +
  labs(x = "Time", y = "Percentage Log Returns",
       title = "GARCH Predictions")

joint_plot <- cowplot::plot_grid(arch.plot, garch.plot, labels = c("A", "B"))
ggsave(joint_plot, filename = "./BayesianAnalysis/arch_garch_plot.pdf", 
       height = 15, width = 30, units = "cm")
