#include <Rcpp.h>
using namespace Rcpp;

//' Manhattan distance between two matrices
//'
//' Implemented in Rcpp
//'
//' @param x Matrix 1
//' @param y Matrix 2
//'
//' @export
//'
// [[Rcpp::export]]
Rcpp::NumericMatrix manhattan_distance (const Rcpp::NumericMatrix & x, const Rcpp::NumericMatrix & y){
  unsigned int nrow = x.nrow();
  unsigned int ncol = y.nrow();
  unsigned int i = 0, j = 0;
  Rcpp::NumericMatrix out(nrow, ncol);
  rownames(out) = rownames(x);
  colnames(out) = rownames(y);

  for (i = 0; i < nrow; i++) {
    Rcpp::NumericVector xi = x.row(i);
    for (j = 0; j < ncol; j++) {
      out(i, j) = sum(abs(xi - y.row(j)));
    }
  }

  return out;
}
