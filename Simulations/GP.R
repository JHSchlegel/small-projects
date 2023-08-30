# Loading Packages and Preprocessing ----
library(tidyverse)
library(rstan)
library(hexbin)

# Gaussian Processes ----
n.sim <- 1000
N <- 500
x <-  seq(-5, 5, l = N)

GP_data <- list(
  N = N,
  x = x,
  alpha = 0.7, 
  rho = 0.5
)

model_script <- "./Simulations/GP.stan"
GP.fit <- stan(file = model_script, data = GP_data, algorithm = "Fixed_param",
               warmup = 0, chains = 1, iter = n.sim)

params <- rstan::extract(GP.fit)

GP_df <- params$f %>% 
  as_tibble() %>% 
  rowid_to_column("iter") %>% 
  pivot_longer(-iter, names_to = "x_id", values_to = "value") %>% 
  mutate(iter = factor(iter),
         x = rep(seq(-5, 5, l = N), n.sim))

quants <- apply(params$f, 2, \(x) quantile(x, c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995)))

GP_df[1:20000,] %>% 
  ggplot(aes(x =x, y = value, group = iter, color = iter)) + 
  geom_line(alpha = 0.5) +
  labs(guides = NULL) +
  theme_bw()

# plot including 90%, 95%, 99% CI
p_intervals <- ggplot() + 
  geom_ribbon(aes(x = x, ymin = quants[1, ], ymax = quants[7, ], fill = "perc99")) + 
  geom_ribbon(aes(x = x, ymin = quants[2, ], ymax = quants[6, ], fill = "perc95")) + 
  geom_ribbon(aes(x = x, ymin = quants[3, ], ymax = quants[5, ], fill = "perc90")) + 
  scale_fill_manual(values = c("#525252", "#989898", "#CCCCCC"),
                    labels = c("90% Interval", "95% Interval", "99% Interval")) +
  geom_line(aes(x = x, y = quants[4,]), color = "black") +
  labs(x = "x", y = "y", title = "Intervals constructed from Guassian Process simulations") +
  theme_bw()

p_intervals
ggsave("./Simulations/Plots/GP_intervals.pdf", plot = p_intervals, width = 6, height = 4)

# Gaussian Processes for Air Passengers ----
data("AirPassengers")
model_script <- "./Simulations/GP_air.stan"

GP_model <- stan_model(model_script)



GP_air <- sampling(GP_model,
                     data = list(
                       N1 = length(AirPassengers),
                       x1 = time(AirPassengers),
                       y1 = as.numeric(AirPassengers),
                       N2 = 50,
                       x2 = seq(range(time(AirPassengers))[1], 1965.238, length.out = 50)
                     ),
                     chains = 1,
                     iter = 2000,
                     show_messages = FALSE,
                     save_warmup = F,
                     seed = 42)

bayesplot::color_scheme_set("viridis")
bayesplot::mcmc_pairs(GP_air, 
                      pars = c("rho", "alpha", "sigma"),
                      diag_fun = "hist", off_diag_fun = "hex")

params <- rstan::extract(GP_air)


pdf("./Simulations/Plots/GP_air_predictions.pdf", width = 6, height = 4)

plot(AirPassengers, xlim = c(range(time(AirPassengers))[1], 1965.238), main = "Predictions stemming from a latent GP model")
for (i in 1:50) {
  lines(seq(range(time(AirPassengers))[1], 1965.238, length.out = 50), params$y2[i,], col = "grey50")
}
lines(seq(range(time(AirPassengers))[1], 1965.238, length.out = 50), apply(params$y2, 2, median), col = "firebrick")

dev.off()
