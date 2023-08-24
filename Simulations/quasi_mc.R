library(qrng)
library(tidyverse)
library(cowplot)
library(ggthemes)

theme_set(ggthemes::theme_economist())

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




