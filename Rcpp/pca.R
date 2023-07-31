library(Rcpp)
library(RcppEigen)
library(microbenchmark)
library(tidyverse)

sourceCpp('pca.cpp')

# create example data with many columns to perform pca on
set.seed(123)
n <- 100
p <- 1000
matx <- matrix(rnorm(n * p), nrow = n, ncol = p)

all.equal(cov_cpp(matx), cov(matx))

# microbenchmark covariance
res_cov <- microbenchmark(
    r = cov(matx),
    cpp = cov_cpp(matx)
)

res_cov.df <- data.frame(
    expr = res_cov$expr, 
    time = res_cov$time
)

ggplot(res_cov.df, aes(x = time, y = expr, fill = expr)) +
    geom_violin() + 
    geom_boxplot(width=0.1, color="black", alpha=0.2) +
    scale_fill_brewer(palette = "Set2") + 
    theme_bw()

# microbenchmark pca
res_pca <- microbenchmark(
    r = prcomp(matx),
    cpp = pca_svd_cpp(matx, 2)
)

res_pca.df <- data.frame(
    expr = res_pca$expr, 
    time = res_pca$time
)

ggplot(res_pca.df, aes(x = time, y = expr, fill = expr)) +
    geom_violin() + 
    geom_boxplot(width=0.1, color="black", alpha=0.2) +
    scale_fill_brewer(palette = "Set2") + 
    theme_bw()
