library(RcppEigen)
library(RcppNumerical)
library(Rcpp)
library(RcppArmadillo)

library(microbenchmark)


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

res <- microbenchmark(
  base = map2_dbl(gridr$i, gridr$j, get_prob),
  cpp = get_sero(nrow(beta), nrow(pop_cat_mat), beta = beta, pop_cat_mat= pop_cat_mat, sigma = sigma),
  times = 100
)

res %>% autoplot



