library(Rcpp)
library(RcppEigen)
library(microbenchmark)

sourceCpp('pca.cpp')

# create example data with many columns to perform pca on
set.seed(123)
n <- 100
p <- 1000
matx <- matrix(rnorm(n * p), nrow = n, ncol = p)

all.equal(cov_cpp(matx), cov(matx))

# microbenchmark variance
res_var <- microbenchmark(
    r = cov(matx),
    cpp = cov_cpp(matx)
)