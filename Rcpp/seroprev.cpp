
#include <RcppArmadillo.h>
#include <iostream>

//[[Rcpp::depends(RcppGSL)]]
#include <RcppGSL.h>
#include <gsl/gsl_integration.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_cdf.h>



//[[Rcpp::depends(RcppArmadillo)]]
//[[Rcpp::export]]
double get_pred(int i, int j, arma::mat beta, arma::mat pop_cat_mat) {
  //std::cout << beta.rows(0,5) << std::endl;
  //std::cout << pop_cat_mat.rows(0,5) << std::endl;
  return arma::dot(beta.row(j), pop_cat_mat.row(i)) ;
};

// #include <RcppNumerical.h>
// using namespace Numer;

// #include <Rcpp.h>


// //[[Rcpp::depends(RcppNumerical)]]
// //[[Rcpp::depends(RcppEigen)]]



// class Seroprev: public Func
// {
// private:
//   const double mu;
//   const double sigma;


// public:
//   Seroprev(double mu_, double sigma_) : mu(mu_), sigma(sigma_) {}

//   double operator()(const double& x) const
//   {
//     return R::plogis(R::qnorm(x, mu, sigma, 1, 0), 0.0, 1.0, 1, 0);
//   }
// };



// //[[Rcpp::export]]
// Rcpp::NumericVector get_sero(int i_max,int j_max,  arma::mat beta, arma::mat pop_cat_mat, arma::vec sigma)
// {
//   double err_est;
//   int err_code;
//   Rcpp::NumericVector res(i_max * j_max);
//   for(int i = 0; i != i_max; ++i) {
//     for(int j = 0; j != j_max; ++j) {
//     double mu = get_pred(i, j, beta, pop_cat_mat);
//     Seroprev f(mu, sigma(j));
//     res(i*j) = Numer::integrate(f, 0.0, 1.0, err_est, err_code);
//     }
//   }
//   return res;
// };




//Using GSL:
struct my_f_params {double mu; double sigma;};

double f1 (double x, void * params) {
  struct my_f_params * fp = (struct my_f_params *) params;
  double mu = (fp->mu);
  double sigma = (fp->sigma);
  return R::plogis(R::qnorm(x, mu, sigma, 1, 0), 0.0, 1.0, 1, 0);
}


double f2 (double x, void * params) {
  struct my_f_params * fp = (struct my_f_params *) params;
  double mu = (fp->mu);
  double sigma = (fp->sigma);
  // gsl gaussian quantile function is defined as:
  // x = mu + sigma * sqrt(2) * erfinv(2y - 1)
  // where y is the cumulative distribution function
  // so we need to convert x to y
  //double q = gsl_cdf_ugaussian_Pinv(x) * sigma + mu;
  //printf("x = %g, mu = %g, sigma = %g, q = %g\n", x, mu, sigma, q);
  //return gsl_cdf_logistic_P(q, 1.0);
  return gsl_cdf_logistic_P(gsl_cdf_ugaussian_Pinv(x) * sigma + mu, 1.0);
}

//[[Rcpp::export]]
//integrate from 0 to 1 using RcppGSL
double gsl_integrate_rdist(double mu, double sigma) {
  gsl_function F;
  //define parameters
  struct my_f_params alpha = {mu, sigma};
  F.function = &f1;
  F.params = &alpha;
  double result, error;
  gsl_integration_workspace * w = gsl_integration_workspace_alloc (1000);
  gsl_integration_qags (&F, 0, 1, 0, 1e-7, 1000, w, &result, &error);
  gsl_integration_workspace_free (w);
  return result;
}

//[[Rcpp::export]]
double gsl_integrate_gsldist(double mu, double sigma) {
  gsl_function F;
  //define parameters
  struct my_f_params alpha = {mu, sigma};
  F.function = &f2;
  F.params = &alpha;
  double result, error;
  gsl_integration_workspace * w = gsl_integration_workspace_alloc (1000);
  gsl_integration_qags (&F, 0, 1, 0, 1e-7, 1000, w, &result, &error);
  gsl_integration_workspace_free (w);
  return result;
}

//[[Rcpp::export]]
//calculating seroprevalence using RcppGSL
/**
 * @brief Get the posterior probabilities
 * 
 * @param i_max i_max is the number of rows in the population category matrix
 * @param j_max j_max is the number of columns in the population category matrix
 * @param beta coefficient matrix
 * @param pop_cat_mat population category matrix
 * @param sigma vector of standard deviations for each population category
 * @param use_rdist whether to use the R dist or the Gsl dist
 * @return Rcpp::NumericVector 
 */
Rcpp::NumericVector get_probs_cpp(int i_max,int j_max,  arma::mat beta, arma::mat pop_cat_mat, 
arma::vec sigma, bool use_rdist = false)
{
  Rcpp::NumericVector res(i_max * j_max);
  if (use_rdist) {
    for(int i = 0; i != i_max; ++i) {
      for(int j = 0; j != j_max; ++j) {
        double mu = get_pred(i, j, beta, pop_cat_mat);
        res(i*j) = gsl_integrate_rdist(mu, sigma(j));
      }
    }
  }
  else {
    for(int i = 0; i != i_max; ++i) {
      for(int j = 0; j != j_max; ++j) {
        double mu = get_pred(i, j, beta, pop_cat_mat);
        res(i*j) = gsl_integrate_gsldist(mu, sigma(j));
      }
    }
  }
  return res;
};

//[[Rcpp::export]]
//get_prob_cpp calculates the probability of seropositivity for a given population category
double get_prob_cpp(int i, int j, arma::mat beta, arma::mat pop_cat_mat, 
                    arma::vec sigma, bool use_rdist = false)
{
  double mu = get_pred(i, j, beta, pop_cat_mat);
  if (use_rdist) {
    return gsl_integrate_rdist(mu, sigma(j));
  }
  else {
    return gsl_integrate_gsldist(mu, sigma(j));
  }
};
