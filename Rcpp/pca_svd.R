library(Rcpp)
library(RcppArmadillo)
library(RcppEigen)
library(tidyverse)

sourceCpp("pca_svd.cpp")

# Generate example data matrix with many columns
X <- matrix(rnorm(1000*1000), nrow=1000, ncol=1000)

# Compare Results of R and C++ implementations
all.equal(get_covariance_matrix(X), cov(X))

svd_r <- svd(X)
svd_cpp <- get_SVD(X)

all.equal(svd_cpp$U, svd_r$u)

all.equal(svd_cpp$V, svd_r$v)

all.equal(svd_cpp$S, svd_r$d)

names(svd(X))
get_pca(X)


dcSVD(X)
