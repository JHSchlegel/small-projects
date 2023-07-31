library(RcppEigen)
library(RcppNumerical)
library(Rcpp)
library(RcppArmadillo)
library(RcppGSL)
library(patchwork)
library(microbenchmark)
library(gridExtra)


sourceCpp("./seroprev.cpp")



beta <- matrix(rnorm(100000), ncol = 5000)
pop_cat_mat <- matrix(rnorm(100000,0, 0.005), ncol = 5000)
sigma <- rexp(100000)


gridr <- expand_grid(i = 1:nrow(pop_cat_mat), j = 1:nrow(beta))
gridcpp <- expand_grid(i = 0:(nrow(pop_cat_mat)-1), j = 0:(nrow(beta)-1))


get_sero(99, 5, beta, pop_cat_mat, sigma)


get_pred(9, 4, beta, pop_cat_mat)
t(beta[5, ]) %*% pop_cat_mat[10, ]



get_prob <- function(i, j){
  integrate(fn <- function(x) {
    plogis(
      qnorm(
        x, 
        t(beta[j, ]) %*% pop_cat_mat[i, ],
        sigma[j]
      )
    )
  }, 0, 1)[[1]]
}

res_scal <- microbenchmark(
  base = map2_dbl(gridr$i, gridr$j, function(i, j) t(beta[j, ]) %*% pop_cat_mat[i, ]),
  cpp = map2_dbl(gridcpp$i, gridcpp$j, get_pred, beta = beta, pop_cat_mat = pop_cat_mat,)
)


base <- map2_dbl(gridr$i, gridr$j, get_prob)
cpp_map_r <- map2_dbl(gridcpp$i, gridcpp$j, get_prob_cpp, beta = beta, pop_cat_mat = pop_cat_mat, sigma = sigma, use_rdist = TRUE)
cpp_map_gsl <- map2_dbl(gridcpp$i, gridcpp$j, get_prob_cpp, beta = beta, pop_cat_mat = pop_cat_mat, sigma = sigma)


# rdist and base r are equal
all.equal(base, cpp_map_r)

# rdist and gsl are equal
all.equal(base, cpp_map_gsl)

# microbenchmark seroprev
res_sero <- microbenchmark(
  base = map2_dbl(gridr$i, gridr$j, get_prob),
  cpp_rdist = get_probs_cpp(nrow(beta), nrow(pop_cat_mat), beta = beta, pop_cat_mat= pop_cat_mat, sigma = sigma, use_rdist = TRUE),
  cpp_gsldist = get_probs_cpp(nrow(beta), nrow(pop_cat_mat), beta = beta, pop_cat_mat= pop_cat_mat, sigma = sigma),
  cpp_map_r = map2_dbl(gridcpp$i, gridcpp$j, get_prob_cpp, beta = beta, pop_cat_mat = pop_cat_mat, sigma = sigma, use_rdist = TRUE),
  cpp_map_gsl = map2_dbl(gridcpp$i, gridcpp$j, get_prob_cpp, beta = beta, pop_cat_mat = pop_cat_mat, sigma = sigma),
  times = 100
)

# convert to data frame
res_sero.df <- data.frame(
  expr = res_sero$expr, 
  time = res_sero$time
)

# violin plot with boxplot overlay for seroprev
p_all <- ggplot(res_sero.df, aes(x = time, y = expr, fill = expr)) +
  geom_violin() + 
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  scale_fill_brewer(palette = "Set2") + 
  theme_bw()

# violin plot for seroprev without base R
p_cpp <- res_sero.df %>% 
  filter(expr != "base") %>% 
  ggplot(aes(x = time, y = expr, fill = expr)) +
  geom_violin() + 
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  scale_fill_brewer(palette = "Set2") + 
  theme_bw()


# plot table of res_sero
p_tab_ser <- ggplot() +
  theme_void() +
  annotation_custom(gridExtra::tableGrob(data.frame(summary(res_sero)), rows = NULL), 
                    xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

# plot grid of p_all and p_cpp using patchwork; label each plot
p_sero_grid <- (p_all + p_cpp) / p_tab_ser
# label all panels of p_sero_grid alphabetically 
p_sero_grid <- p_sero_grid + plot_annotation(tag_levels = "A")
ggsave("p_sero_grid.pdf", p_sero_grid, width = 30, height = 20, units = "cm")
