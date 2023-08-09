#include <Rcpp.h>
#include <cmath>
// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
using namespace Rcpp;



template <class T>
double myMean(T &x)
{
    double res = 0;
    int n = x.size();
    for (int i = 0; i<n; i++) {
        res += x[i];
    }
    return res/n;
};

template <class T>
double myVar (T &x)
{
    double mu = myMean(x);
    int n = x.size();
    double res = 0;
    for (int i = 0; i < x.size(); i++) {
        res += pow(x[i]-mu, 2);
    }
    return res/(n-1);
};

template <class T>
double mySd(T &x)
{
    return sqrt(myVar(x));
};
//[[Rcpp::export]]
/**
 * @brief Calculates mean of a numeric vector
 * 
 * @param x numeric vector
 * @return double 
 */
double meanC(NumericVector x)
{
    return myMean(x);
}


//[[Rcpp::export]]
/**
 * @brief Calculates variance of a numeric vector
 * 
 * @param x numeric vector
 * @return double 
 */
double varC(NumericVector x)
{
    return myVar(x);
}

//[[Rcpp::export]]
/**
 * @brief Calculates standard deviation of a numeric vector
 * 
 * @param x numeric vector
 * @return double 
 */
double sdC(NumericVector x)
{
    return mySd(x);
}


//[[Rcpp::export]]
/**
 * @brief Calculates mean of a numeric matrix
 * 
 * @param x numeric matrix
 * @return NumericVector 
 */
NumericVector colVar(NumericMatrix x) 
{
    int ncols = x.ncol();
    NumericVector res(ncols);
    for (int i = 0; i < ncols; i++){
        NumericMatrix::Column tmp = x(_, i);
        res[i] = myVar(tmp);
    }
    return res;
}



//[[Rcpp::export]]
/**
 * @brief Get the normal samples object
 * 
 * @param n number of samples
 * @param mu mean vector
 * @param sigma covariance matrix
 * @return NumericMatrix 
 */
NumericMatrix get_normal_samples(int n, NumericVector mu, NumericMatrix sigma)
{
    Environment pkg = Environment::namespace_env("mvtnorm");
    Function f = pkg["rmvnorm"];
    return f(n, mu, sigma);
}





//calculate variance of a matrix rowwise
//[[Rcpp::export]]
/**
 * @brief Calculates variance of a numeric matrix
 * 
 * @param x numeric matrix
 * @return NumericVector 
 */
NumericVector rowVar(NumericMatrix x) 
{
    int nrows = x.nrow();
    NumericVector res(nrows);
    for (int i = 0; i < nrows; i++){
        NumericMatrix::Row tmp = x(i, _);
        res[i] = myVar(tmp);
    }
    return res;
}



using namespace RcppParallel;

// construct the worker for parallel execution and execute the algorithm
struct colVarWorker : public Worker
{
    // source matrix
    const RMatrix<double> input;
    // destination matrix
    RVector<double> output;
    // initialize from Rcpp input and output matrixes (the RMatrix class
    // can be automatically converted to from the Rcpp matrix type)
    colVarWorker(const NumericMatrix input, NumericVector output)
        : input(input), output(output) {}
    void operator()(std::size_t begin, std::size_t end)
    {
        for (std::size_t i = begin; i < end; i++) {
            RMatrix<double>::Column tmp = input.column(i);
            output[i] = myVar(tmp);
        }
    }
};

//[[Rcpp::export]]
//calculate variance of a matrix columnwise in parallel using rcppparallel
/**
 * @brief Calculates variance of a numeric matrix columnwise in parallel
 * 
 * @param x numeric matrix
 * @return NumericVector 
 */
NumericVector colVarParallel(NumericMatrix x) 
{
    int ncols = x.ncol();
    NumericVector res(ncols);
    colVarWorker colVarWorker(x, res);
    parallelFor(0, ncols, colVarWorker);
    return res;
}








