# This file demonstrates important differences between quasi and pseudo random
# numbers

# Packages and Presets ----

library(qrng)
library(tidyverse)
library(cowplot)
library(ggthemes)

theme_set(ggthemes::theme_economist())

# Halton Sequence ----
#' @description Halton sequence is a quasi random number generator. It is a low discrepancy
#' sequence. It is a deterministic sequence. It is a sequence of numbers in the
#' unit interval that are equidistributed. 
#' @param k index of the sequence
#' @param b The base of the sequence
halton <- function(k, b){
  res <- 0
  f <- 1
  while (k > 0){
    f <- f / b
    res <- res + f * (k %% b)
    k <- floor(k / b)
  }
  return(res)
}

sapply(1:1000, halton, b = 2) %>% 
data.frame() %>% 
  ggplot(aes(x = .)) + 
  geom_histogram(bins = 20) +
  labs(x = "x", y = "Frequency", title = "Halton Sequence") +
  theme()


#' Halton sequence using only integer numbers to 
#' make it robust against round off errors
halton_int <- function(nsim, b){
  res <- numeric(nsim)
  n <- 0
  d <- 1
  for (i in 1:nsim){
    x <- d - n
    if (x == 1){
      n <- 1 
      d <- d * b
    }
    else {
      y <- d %/% b
      while (x <= y) {
        y <- y %/% b
      }
      n <- (b + 1) * y - x
    }
  res[i] <- n / d
  }
  return(res)
} 

plot(halton_int(1000, 2), halton_int(1000, 3))

# Plotting Quasi and Pseudo Random Numbers ----
set.seed(123)
rng.df <- data.frame(x.pr = runif(1000), y.pr = runif(1000))
rng.df[, c("x.qr", "y.qr")] <- qrng::ghalton(1000, d = 2)

p_pr <- rng.df %>% 
  ggplot(aes(x = x.pr, y = y.pr)) + 
  geom_point(alpha = 0.5) +
  labs(x = "x", y = "y", title = "Pseudo Random Numbers") +
  coord_cartesian(expand = FALSE, xlim = c(0, 1), ylim = c(0, 1)) +
  theme()

p_qr <- rng.df %>% 
  ggplot(aes(x = x.qr, y = y.qr)) + 
  geom_point(alpha = 0.5) +
  labs(x = "x", y = "y", title = "Quasi Random Numbers") +
  coord_cartesian(expand = FALSE, xlim = c(0, 1), ylim = c(0, 1))

p_combined <- plot_grid(p_pr, p_qr) 
p_combined

ggsave("./Simulations/Plots/qrng_vs_prng.pdf", plot = p_combined, width = 10,
       height = 4.5)



# Monte Carlo Integration ---- 
# Let's calculate expectation of exp(abs(x - 2))
# the interval [-1, 3]

n.sim <- 1000000

set.seed(123)
X.qr <- -1 + 4 * qrng::ghalton(n.sim)

set.seed(123)
X.pr <- runif(n.sim, -1, 3)


Y <- function(x) 4 * exp(-abs(x-2))

mcint.df <- data.frame(X.qr = X.qr, Y.qr = Y(X.qr), X.pr = X.pr, Y.pr = Y(X.pr))


mcint_long <- mcint.df %>% 
  rowid_to_column("n") %>% 
  pivot_longer(-n, names_to = "type", values_to = "value")

mcint_long %>% 
  group_by(type) %>% 
  summarize(
    mean = mean(value),
    sd = sd(value),
    p25 = quantile(value, 0.25),
    median = median(value),
    p75 = quantile(value, 0.75)
  )

mcint.df %>% 
  ggplot(aes(x = X.qr, y = Y.qr)) + 
  geom_line()

# Importance Sampling ----














