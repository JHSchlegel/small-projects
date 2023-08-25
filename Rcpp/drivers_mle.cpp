#include <TMB.hpp>


// [[Rcpp::export]]
/**
 * @brief Template Model Builder for MLE
 * 
 * @tparam Type
 * @return Type
 */
template<class Type>
Type objective_function<Type>::operator() ()
{
    //data
    DATA_VECTOR(x);
    //parameters
    PARAMETER(mu);
    PARAMETER(sigma);
    //objective function
    Type nll = -sum(dnorm(x, mu, sigma, true));
    return nll;
};
