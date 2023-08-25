////[[Rcpp::depends(RcppEigen)]]
//#include <RcppEigen.h>

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>


// [[Rcpp::export]]
arma::vec dcSVD(const arma::mat & X) {
    arma::mat U, V;
    arma::vec S;
    arma::svd(U, S, V, X, "dc");
    return S;
}

// calulate covariance matrix
//// [[Rcpp::export]]
// Eigen::MatrixXd get_covariance_matrix(Eigen::MatrixXd x)
// {
//     Eigen::MatrixXd centered = x.rowwise() - x.colwise().mean();
//     Eigen::MatrixXd cov = (centered.adjoint() * centered) / double(x.rows() - 1);
//     return cov;
// }


//// [[Rcpp::export]]
// calculate eigenvalues and eigenvectors using SVD
// /**
//  * @brief Main function to calculate eigenvalues and eigenvectors using SVD
//  * 
//  * @param x NumericMatrix
//  * @return Rcpp::List of U, V, S
//  */
// Rcpp::List get_SVD(Eigen::MatrixXd x)
// {
//     Eigen::JacobiSVD<Eigen::MatrixXd> svd(x, Eigen::ComputeFullU | Eigen::ComputeFullV);
//     Eigen::MatrixXd U = svd.matrixU();
//     Eigen::MatrixXd V = svd.matrixV();
//     Eigen::VectorXd S = svd.singularValues();
//     return Rcpp::List::create(Rcpp::Named("U") = U,
//                               Rcpp::Named("V") = V,
//                               Rcpp::Named("S") = S);
// }