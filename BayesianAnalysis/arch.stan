//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//


data {
  int<lower=0> T;// nr of datapoints
  real r[T]; // data
}


parameters {
  real mu;// average return
  real <lower = 0> alpha0; //noise intercept
  real<lower=0, upper = 1> alpha1;//noise slope
}


model {
  // priors
  mu ~ normal(0,10);
  alpha0 ~ normal(0, 10);
  alpha1 ~ normal(0, 10);
  //likelihood
  for (t in 2:T){
    r[t] ~ normal(mu, sqrt(alpha0 + alpha1 * pow(r[t-1] - mu, 2)));
  }
}

