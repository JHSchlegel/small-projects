//simulate GP with RBF kernel
data{
  int<lower = 1> N; //nr of data points
  real x[N]; //data
  real<lower=0> alpha;
  real<lower=0> rho;
}

transformed data{
  //cov function; add constant to diagonal to ensure matrix is positive definite
  matrix[N, N] K = cov_exp_quad(x, alpha, rho) + diag_matrix(rep_vector(1e-10, N));
  //mean 
  vector[N] mu = rep_vector(0, N);
}

generated quantities {
  vector[N] f = multi_normal_rng(mu, K);
}