
data {
  int<lower=0> T;// nr of datapoints
  real r[T]; // data
  real<lower=0> sigma1; //scale of noise at t=1
  real<lower=0> ind1;
}


parameters {
  real mu;// average return
  real<lower=0> alpha0; // intercept
  real<lower=0, upper=1> alpha1;//slope on location
  real<lower=0, upper=1> beta1;//slope volatility
  real<lower=0, upper=2-2*alpha1-2*beta1> gamma;//captures volatility clustering
}

transformed parameters{
  real<lower=0> sigma[T];
  real<lower=0> ind[T];
  sigma[1] = sigma1;
  ind[1] = ind1;
  for (t in 2:T){
    ind[t] = (r[t-1] >= mu) ? 0: 1;
    sigma[t] = sqrt(alpha0+ (alpha1 + gamma * ind[t]) * pow(r[t-1]-mu, 2) + beta1 * pow(sigma[t-1], 2)); //error term
  }
}


model {
  // priors
  mu ~ normal(0,10);
  alpha0 ~ normal(0, 10);
  alpha1 ~ normal(0, 10);
  beta1 ~ normal(0, 10);
  gamma ~ normal(0, 5);
  //likelihood
  r ~ normal(mu, sigma);
}
