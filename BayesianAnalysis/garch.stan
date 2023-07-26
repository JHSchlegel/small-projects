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
  real<lower=0> sigma1; //scale of noise at t=1
}


parameters {
  real mu;// average return
  real<lower=0> alpha0; // intercept
  real<lower=0, upper=1> alpha1;//slope on location
  real<lower=0, upper=(1-alpha1)> beta1;//slope volatility
}

transformed parameters{
  real<lower=0> sigma[T];
  sigma[1] = sigma1;
  for (t in 2:T){
    sigma[t] = sqrt(alpha0+ alpha1 * pow(r[t-1]-mu, 2) + beta1 * pow(sigma[t-1], 2)); //error term
  }
}


model {
  // priors
  mu ~ normal(0,10);
  alpha0 ~ normal(0, 10);
  alpha1 ~ normal(0, 10);
  beta1 ~ normal(0, 10);
  //likelihood
  r ~ normal(mu, sigma);
}

