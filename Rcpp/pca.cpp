//[[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>

// [[Rcpp::export]]
//calculate covariance matrix using Eigen
Eigen::MatrixXd cov_cpp(const Eigen::MatrixXd& X) {
  Eigen::MatrixXd centered = X.rowwise() - X.colwise().mean();
  Eigen::MatrixXd cov = (centered.adjoint() * centered) / double(X.rows() - 1);
  return cov;
}

// [[Rcpp::export]]
//calculate eigenvalues and eigenvectors using Eigen
Rcpp::List eigen_cpp(const Eigen::MatrixXd& X) {
  Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> eigensolver(X);
  Eigen::VectorXd eigenvalues = eigensolver.eigenvalues();
  Eigen::MatrixXd eigenvectors = eigensolver.eigenvectors();
  return Rcpp::List::create(Rcpp::Named("eigenvalues") = eigenvalues,
                            Rcpp::Named("eigenvectors") = eigenvectors);
}

// [[Rcpp::export]]
//calculate principal components using Eigen
Eigen::MatrixXd pca_cpp(const Eigen::MatrixXd& X, const int& n) {
  Eigen::MatrixXd centered = X.rowwise() - X.colwise().mean();
  Eigen::MatrixXd cov = (centered.adjoint() * centered) / double(X.rows() - 1);
  Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> eigensolver(cov);
  Eigen::VectorXd eigenvalues = eigensolver.eigenvalues();
  Eigen::MatrixXd eigenvectors = eigensolver.eigenvectors();
  Eigen::MatrixXd principal_components = centered * eigenvectors.rightCols(n);
  return principal_components;
}

// [[Rcpp::export]]
//calculate singular values and singular vectors using Eigen
Rcpp::List svd_cpp(const Eigen::MatrixXd& X) {
  Eigen::JacobiSVD<Eigen::MatrixXd> svd(X, Eigen::ComputeThinU | Eigen::ComputeThinV);
  Eigen::VectorXd singular_values = svd.singularValues();
  Eigen::MatrixXd singular_vectors = svd.matrixV();
  return Rcpp::List::create(Rcpp::Named("singular_values") = singular_values,
                            Rcpp::Named("singular_vectors") = singular_vectors);
}

// [[Rcpp::export]]
//calculate principal components using singular value decomposition and Eigen
Eigen::MatrixXd pca_svd_cpp(const Eigen::MatrixXd& X, const int& n) {
  Eigen::JacobiSVD<Eigen::MatrixXd> svd(X, Eigen::ComputeThinU | Eigen::ComputeThinV);
  Eigen::VectorXd singular_values = svd.singularValues();
  Eigen::MatrixXd singular_vectors = svd.matrixV();
  Eigen::MatrixXd principal_components = X * singular_vectors.rightCols(n);
  return principal_components;
}