# Package Loading and Preprocessing ---------------------------------------
library(rstan)
library(parallel)
library(coda) # for gelman plot
library(hexbin)
library(tidyverse)
library(cowplot)

# load("./Simulations/gjr_garch.rds")

ts <- data.frame(year <- time(EuStockMarkets))
stocks <- data.frame(EuStockMarkets)

stocks.plrets <- apply(stocks, 2, function(x) 100 * diff(log(x), 1))
y <- stocks.plrets[, 4] # FTSE
dim(stocks)
dim(stocks.plrets)


plot(ts[-length(ts),], y, type = "h")
abline(h = 0, col = "lightgrey")



numcores <- parallel::detectCores()
options(mc.cores = numcores - 4)



# GJR-GARCH Parameter Simulation -----------------------------------------------
gjr_garch_script <- "./Simulations/gjr_garch.stan"
gjr_garch_model <- stan_model(gjr_garch_script)



gjr_garch.post <- sampling(gjr_garch_model,
                      data = list(
                        T = length(y), 
                        r = y, 
                        sigma1 = 0.1, 
                        ind1 = ifelse(y[length(y)] >= y[length(y)-1], 0, 1)
                      ),
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
# save(gjr_garch.post, file = "./Simulations/gjr_garch.rds")
# trace plots
par(mfrow = c(3, 2))
rstan::traceplot(gjr_garch.post, 
                 pars = c("mu", "alpha0", "alpha1", "beta1", "gamma"))
# summary statistics
rstan::summary(gjr_garch.post, 
               pars = c("mu", "alpha0", "alpha1", "beta1", "gamma"))

# Gelman plots
gelman.plot(As.mcmc.list(gjr_garch.post, 
                         pars = c("mu", "alpha0", "alpha1", "beta1", "gamma")))
# some convergence issues between chains for mu

# ACF plots over all chains
postpars.mat <- as.matrix(gjr_garch.post)[, 1:5]
par(mfrow = c(3,2))
for (i in 1:5){
  acf(acf(postpars.mat[, i], plot = F, lag = 1000)$acf, lag = 1000,
      main = c("mu", "alpha0", "alpha1", "gamma")[i])
}

# density ridges
bayesplot::color_scheme_set("brightblue")
bayesplot::mcmc_areas_ridges(gjr_garch.post, 
                            pars = c("mu", "alpha0", "alpha1", "beta1", "gamma"))

# chainwise ACF plots
bayesplot::mcmc_acf(gjr_garch.post, 
                    pars = c("mu", "alpha0", "alpha1", "beta1", "gamma"))

# 95% CI
bayesplot::color_scheme_set("red")
bayesplot::mcmc_intervals(gjr_garch.post, 
                          pars = c("mu", "alpha0", "alpha1", "beta1"),
                          prob_outer = 0.95, point_est = "mean")

# histograms
bayesplot::mcmc_hist(gjr_garch.post, 
                     pars = c("mu", "alpha0", "alpha1", "beta1", "gamma"))

# density overlays of all chains
bayesplot::color_scheme_set("teal")
bayesplot::mcmc_dens_overlay(gjr_garch.post,
                          pars = c("mu", "alpha0", "alpha1", "beta1", "gamma"))

# violin plots for all chains
bayesplot::color_scheme_set("pink")
bayesplot::mcmc_violin(gjr_garch.post,
                       pars = c("mu", "alpha0", "alpha1", "beta1", "gamma"))

# pairs plot
bayesplot::color_scheme_set("viridis")
bayesplot::mcmc_pairs(gjr_garch.post, 
                      pars = c("mu", "alpha0", "alpha1", "beta1", "gamma"),
                      diag_fun = "hist", off_diag_fun = "hex")

# alpha1 vs gamma
bayesplot::color_scheme_set("viridis")
bayesplot::mcmc_hex(gjr_garch.post, pars = c("alpha1", "gamma"))


garch.params <- rstan::extract(gjr_garch.post)

mu.mean <- mean(garch.params$mu)
alpha0.mean <- mean(garch.params$alpha0)
alpha1.mean <- mean(garch.params$alpha1)
beta1.mean <- mean(garch.params$beta1)
gamma.mean <- mean(garch.params$gamma)
sigma.mean <- colMeans(garch.params$sigma)
ind.mean <- ifelse(colMeans(garch.params$ind) > 0.5, 1, 0)


mu <- garch.params$mu
alpha0 <- garch.params$alpha0
alpha1 <- garch.params$alpha1
beta1 <- garch.params$beta1
gamma <- garch.params$gamma
sigma <- garch.params$sigma
ind <- garch.params$ind

# predicted volatility
pred.gjr_garch <- sapply(2:1860, function(x) 
  mu.mean + sqrt(alpha0.mean + (alpha1.mean + gamma.mean * 
                             ind.mean[x-1]) * (y[x-1] - mu.mean)^2 + 
              beta1.mean * sigma.mean[x-1]^2)
  )

VaR.gjr_garch <- sapply(3:1860, 
        function(x) quantile(mu + sqrt(alpha0 + (alpha1 + gamma * ind[, x-1]) * 
                                         (y[x-1] - mu)^2 + 
                                         beta1 * sigma[, x-1]^2), c(.99, .95)))


# Plotting GJR-GARCH Predictions --------------------------------------------------
gjr_garch.df <- data.frame(
  time = ts[-c(1, nrow(ts)),],
  plrets = y[-1], 
  pred = pred.gjr_garch, 
  neg_pred = -pred.gjr_garch,
  VaR_01 = VaR.gjr_garch[1, ],
  VaR_05 = VaR.gjr_garch[2, ]
  ) %>% 
  mutate(
    exceed_01 = ifelse(plrets < -VaR_01, 1, 0),
    exceed_05 = ifelse(plrets < -VaR_05, 1, 0)
  )


gjr_garch.plot <- gjr_garch.df %>% 
  ggplot() +
  geom_line(aes(x = time, y = plrets, color = "plrets")) + 
  geom_line(aes(x = time, y = pred, color = "pred")) +
  geom_line(aes(x = time, y = neg_pred, color = "pred")) +
  geom_hline(aes(yintercept = 0), alpha = .5, color = "lightgrey") +
  scale_colour_manual(values = c("black", "red"),
                      labels = c("percentage log returns", "GJR-GARCH volatility predictions")) +
  theme_bw() +
  guides(color = guide_legend(title = "")) +
  labs(x = "Time", y = "Percentage Log Returns",
       title = "GARCH Predictions")

gjr_garch.plot

## expected number of exceedances:
# 1%:
1859 * 0.01 # 19
1859 * 0.05 # 93
## number of exceedances:
sum(gjr_garch.df$exceed_01)
sum(gjr_garch.df$exceed_05)

plot(1:1859, gjr_garch.df$VaR_01, type = "l")

VaR.plot <- gjr_garch.df %>% 
  ggplot() +
  geom_point(aes(x = time, y = plrets, color = "plrets"), alpha = 0.5, size = 0.5) + 
  geom_line(aes(x = time, y = -VaR_01, color = "VaR01")) +
  geom_line(aes(x = time, y = -VaR_05, color = "VaR05")) +
  scale_colour_manual(values = c("grey40", "#55ecbd", "#411142"),
                      labels = c("percentage log returns", "1% VaR", "5% VaR")) +
  theme_bw() +
  guides(color = guide_legend(title = "")) +
  labs(x = "Time", y = "Percentage Log Returns",
       title = "GARCH Predictions")
VaR.plot
