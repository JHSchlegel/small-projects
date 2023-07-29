
#include <RcppArmadillo.h>
#include <iostream>



//[[Rcpp::depends(RcppArmadillo)]]
//[[Rcpp::export]]
double get_pred(int i, int j, arma::mat beta, arma::mat pop_cat_mat) {
  //std::cout << beta.rows(0,5) << std::endl;
  //std::cout << pop_cat_mat.rows(0,5) << std::endl;
  return arma::dot(beta.row(j), pop_cat_mat.row(i)) ;
};

#include <RcppNumerical.h>
using namespace Numer;

#include <Rcpp.h>


//[[Rcpp::depends(RcppNumerical)]]
//[[Rcpp::depends(RcppEigen)]]


class Seroprev: public Func
{
private:
  const double mu;
  const double sigma;


public:
  Seroprev(double mu_, double sigma_) : mu(mu_), sigma(sigma_) {}

  double operator()(const double& x) const
  {
    return R::plogis(R::qnorm(x, mu, sigma, 1, 0), 0.0, 1.0, 1, 0);
  }
};



//[[Rcpp::export]]
Rcpp::NumericVector get_sero(int i_max,int j_max,  arma::mat beta, arma::mat pop_cat_mat, arma::vec sigma)
{
  double err_est;
  int err_code;
  Rcpp::NumericVector res(i_max * j_max);
  for(int i = 0; i != i_max; ++i) {
    for(int j = 0; j != j_max; ++j) {
    double mu = get_pred(i, j, beta, pop_cat_mat);
    Seroprev f(mu, sigma(j));
    res(i*j) = Numer::integrate(f, 0.0, 1.0, err_est, err_code);
    }
  }
  return res;
};

