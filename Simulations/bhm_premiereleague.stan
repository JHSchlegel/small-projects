//
// This Stan program defines a simple model, with a

data {
  int<lower=0> n_teams;//number of teams
  int<lower=0> n_games; //number of games
  int<lower=0> home[n_games]; //home team
  int<lower=0> away[n_games]; //away team
  int<lower=0> score1[n_games]; //score home team
  int<lower=0> score2[n_games]; //score away team
  int<lower=0> n_predict; //number of predicted games
  int<lower=0> home_new[n_predict]; //home team index for prediction
  int<lower=0> away_new[n_predict];
}


parameters {
  real home_adv; //home advantage
  vector[n_teams] attack; //attack ability of each team
  vector[n_teams] defense; //defense ability of each team
  //hyper parameters
  real mu_attack;
  real<lower=0> tau_attack;
  real mu_defense;
  real<lower=0> tau_defense;
}

transformed parameters{
  vector[n_games] theta1; //score probability of home team
  vector[n_games] theta2; //score probability of away team
  
  theta1 = exp(home_adv + attack[home] - defense[away]);
  theta2 = exp(attack[away] - defense[home]);
}

model {
  //hyperpriors
  mu_attack ~ normal(0, 0.1);
  tau_attack ~ normal(0, 1);
  mu_defense ~ normal(0, 0.1);
  tau_defense ~ normal(0, 1);
  //priors 
  attack ~ normal(mu_attack, tau_attack);
  defense ~ normal(mu_defense, tau_defense);
  home_adv ~ normal(0, 0.001);
  //likelihood
  score1 ~ poisson(theta1);
  score2 ~ poisson(theta2);
}


generated quantities {
  // generate predictions
  vector[n_predict] theta1new;//score probability of home team
  vector[n_predict] theta2new;//score probability of away team
  real score1new[n_predict]; // predicted score1
  real score2new[n_predict]; // predicted score2
  
  theta1new = exp(home_adv + attack[home_new] - defense[away_new]);
  theta2new = exp(attack[away_new] - defense[home_new]);
}
