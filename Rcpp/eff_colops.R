library(Rcpp)
library(RcppParallel)
RcppParallel::setThreadOptions(numThreads = defaultNumThreads() - 4)
library(microbenchmark)
library(ggplot2)
library(viridis)
library(mvtnorm)
library(cowplot)
library(flextable)



sourceCpp('adv_r_ex.cpp')

x <- rnorm(10000000, 2, 5)

matx <- matrix(rnorm(100 * 100000), ncol = 100000)

meanC(x)
varC(x)
sdC(x)
colVar(matx)
rowVar(matx)
colVarParallel(matx)


# microbenchmark mean
res_mean <- microbenchmark(
    r = mean(x),
    cpp = meanC(x)
)

res_mean.df <- data.frame(
    expr = res_mean$expr, 
    time = res_mean$time
)
ggplot(res_mean.df, aes(x = time, y = expr, fill = expr)) +
    geom_violin() + 
    scale_fill_brewer(palette = "Set2") + 
    theme_bw()

# microbenchmark var
res_var <- microbenchmark(
    r = mean(x),
    cpp = meanC(x)
)

res_var.df <- data.frame(
    expr = res_var$expr, 
    time = res_var$time
)
ggplot(res_var.df, aes(x = time, y = expr, fill = expr)) +
    geom_violin() + 
    geom_boxplot(width=0.1, color="black", alpha=0.2) +
    scale_fill_brewer(palette = "Set2") + 
    theme_bw()



# microbenchmark columnwise variance
res_col_var <- microbenchmark(
    r = apply(matx, 2, var),
    cpp = colVar(matx),
    cpp_parallel = colVarParallel(matx)
)
res_col_var


# create table of res_col_var using ggplot and annotation_custom
tab_col_var <- ggplot() +
theme_void() +
annotation_custom(gridExtra::tableGrob(summary(res_col_var), rows = NULL), 
        xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)


# cpp_parallel is way faster than cpp and r
res_col_var.df <- data.frame(
    expr = res_col_var$expr, 
    time = res_col_var$time
)
# plot results
p_colvar <- ggplot(res_col_var.df, aes(x = time, y = expr, fill = expr)) +
    geom_violin() + 
    geom_boxplot(width=0.1, color="black", alpha=0.2) +
    scale_fill_brewer(palette = "Set2") + 
    theme_bw() +
    labs(y = "language", title = "Column Variance Calculation")
p_colvar


# microbenchmark multivariate normal
n <- 100
mu <- c(10, 20)
sigma <- matrix(c(2, -1, -1, 3), nrow = 2)
get_normal_samples(n, mu, sigma)

set.seed(123)
res_mvnorm <- microbenchmark(
    r = rmvnorm(n, mu, sigma),
    cpp = get_normal_samples(n, mu, sigma)
)
# cpp only slighlty slower

res_mvnorm.df <- data.frame(
    expr = res_mvnorm$expr, 
    time = res_mvnorm$time
)
p_mvnorm <- ggplot(res_mvnorm.df, aes(x = time, y = expr, fill = expr)) +
    geom_violin() + 
    geom_boxplot(width=0.1, color="black", alpha=0.2) +
    scale_fill_brewer(palette = "Set2") + 
    theme_bw() + 
    labs(y = "language", title = "Calling Function From R package")
p_mvnorm


comb_colvar <- cowplot::plot_grid(p_colvar, tab_col_var, nrow = 2,rel_heights = c(1, .2))
comb_colvar
p_colvar_mvnorm <- cowplot::plot_grid(comb_colvar, p_mvnorm, labels = "AUTO", rel_widths = c(1.5, 1))
p_colvar_mvnorm
ggsave("rcpp_speed.pdf", plot = p_colvar_mvnorm, height = 20, width = 30, units = "cm")


