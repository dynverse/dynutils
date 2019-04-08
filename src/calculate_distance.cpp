#include <Rcpp.h>
using namespace Rcpp;

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

// [[Rcpp::export]]
Rcpp::NumericMatrix euclidean_distance (const Rcpp::NumericMatrix & x, const Rcpp::NumericMatrix & y){
  unsigned int nrow = x.nrow();
  unsigned int ncol = y.nrow();
  unsigned int i = 0, j = 0;
  Rcpp::NumericMatrix out(nrow, ncol);
  rownames(out) = rownames(x);
  colnames(out) = rownames(y);

  for (i = 0; i < nrow; i++) {
    Rcpp::NumericVector xi = x.row(i);
    for (j = 0; j < ncol; j++) {
      out(i, j) = sqrt(sum(pow(xi - y.row(j), 2)));
    }
  }

  return out;
}

// /*
//  * Interpreted from
//  * https://github.com/cran/wordspace/blob/bded405974e84dc08c95694422092d40df1907fd/src/wordspace.cpp#L108-L286
//  */
//
// /* internal codes for metric / distance:
//  *  0 = euclidean
//  *  1 = maximum
//  *  2 = manhattan
//  *  3 = minkowski   (*param1 = exponent p)
//  *  4 = canberra
//  */
//
// /* make symmetric matrix from right upper triangle */
// void mk_symmetric_matrix(NumericMatrix x) {
//   int nc = x.ncol();
//   for (int c = 0; c < nc; c++)
//     for (int r = 0; r < c; r++)
//       x(c, r) = x(r, c);
// }
//
// /* validate metric code and parameter(s) */
// void check_metric(int metric_code, double p1) {
//   if (metric_code < 0 || metric_code > 4)
//     stop("internal error -- invalid metric code");
//   if (metric_code == 3 && (!R_FINITE(p1) || p1 < 0.0))
//     stop("internal error -- Minkowski p-parameter out of range [0, Inf)");
// }
//
// // [[Rcpp::export]]
// NumericMatrix CPP_dist_dense(const Rcpp::NumericMatrix x, const Rcpp::NumericMatrix y, int metric_code, double param1, bool symmetric) {
//   check_metric(metric_code, param1);
//
//   int nr = x.nrow(), nc1 = x.ncol(), nc2 = y.ncol();
//   if (nr != y.nrow()) stop("internal error -- matrices are not conformable");
//
//   NumericMatrix dist(nc1, nc2);
//
// #pragma omp parallel for                                                    \
//   if (openmp_threads > 1 && (nc1 + 0.0) * (nc2 + 0.0) * (nr + 0.0) > 100e6) \
//     num_threads(openmp_threads)                                             \
//     shared(dist)
//     for (int col2 = 0; col2 < nc2; col2++) {
//       NumericVector tmp(nr);
//       double accum;
//       int col1_max = (symmetric) ? col2 + 1 : nc1;
//       for (int col1 = 0; col1 < col1_max; col1++) {
//         NumericVector
//         NumericMatrix::Column vx = x(_, col1);  // column <col1> of first matrix
//         NumericMatrix::Column vy = y(_, col2);  // column <col2> of second matrix
//         switch (metric_code) {
//         case 0:
//           accum = sum((vx - vy) * (vx - vy));
//           dist(col1, col2) = sqrt(accum);
//           break;
//         case 1:
//           dist(col1, col2) = max(abs(vx - vy));
//           break;
//         case 2:
//           dist(col1, col2) = sum(abs(vx - vy));
//           break;
//         case 3:
//           accum = sum(pow(abs(vx - vy), param1));
//           if (param1 > 1.0)
//             dist(col1, col2) = pow(accum, 1.0 / param1);
//           else
//             dist(col1, col2) = accum;
//           break;
//         case 4:
//           tmp = abs(vx) + abs(vy); // denominator |x_i| + |y_i|
//           dist(col1, col2) = sum(ifelse(tmp > 0, abs(vx - vy) / tmp, 0.0));
//           break;
//         }
//       }
//     }
//
//     if (symmetric) mk_symmetric_matrix(dist);
//     return dist;
// }
//
//
// // [[Rcpp::export]]
// NumericMatrix CPP_col_dist_sparse(int nc1, IntegerVector xp, IntegerVector xrow, NumericVector x, int nc2, IntegerVector yp, IntegerVector yrow, NumericVector y, int metric_code, double param1, bool symmetric) {
//   check_metric(metric_code, param1);
//   NumericVector::iterator _x = x.begin();
//   NumericVector::iterator _y = y.begin();
//   IntegerVector::iterator _xrow = xrow.begin();
//   IntegerVector::iterator _yrow = yrow.begin();
//
//   NumericMatrix dist(nc1, nc2);
//
// #ifdef _OPENMP
//   /* average number of entries scanned when comparing two columns, used to decide whether to try parallelization */
//   double avg_nr = (xp[nc1] - xp[0] + 0.0) / nc1 + (yp[nc2] - yp[0] + 0.0) / nc2;
// #endif
//
// #pragma omp parallel for                                               \
//   if (openmp_threads > 1 && (nc1 + 0.0) * (nc2 + 0.0) * avg_nr > 40e6) \
//     num_threads(openmp_threads)                                        \
//     shared(dist, nc1, xp, _xrow, _x, nc2, yp, _yrow, _y, metric_code, param1)
//     for (int col2 = 0; col2 < nc2; col2++) {
//       int col1_max = (symmetric) ? col2 + 1 : nc1;
//       int yi_max = yp[col2 + 1];
//
//       for (int col1 = 0; col1 < col1_max; col1++) {
//         int xi_max = xp[col1 + 1];
//         int xi = xp[col1];
//         int yi = yp[col2];
//         int xrow_curr = (xi < xi_max) ? _xrow[xi] : INT_MAX;
//         int yrow_curr = (yi < yi_max) ? _yrow[yi] : INT_MAX;
//
//         double accum = 0.0;
//         double x_curr, y_curr;
//         double d_xy, x_plus_y;
//         while (xi < xi_max || yi < yi_max) {
//
//           if (xrow_curr < yrow_curr) {
//             x_curr = _x[xi]; y_curr = 0.0;
//             xi++;
//             xrow_curr = (xi < xi_max) ? _xrow[xi] : INT_MAX;
//           }
//           else if (xrow_curr == yrow_curr) {
//             x_curr = _x[xi]; y_curr = _y[yi];
//             xi++; yi++;
//             xrow_curr = (xi < xi_max) ? _xrow[xi] : INT_MAX;
//             yrow_curr = (yi < yi_max) ? _yrow[yi] : INT_MAX;
//           }
//           else /* xrow_curr > yrow_curr */ {
//             x_curr = 0; y_curr = _y[yi];
//             yi++;
//             yrow_curr = (yi < yi_max) ? _yrow[yi] : INT_MAX;
//           }
//
//           switch (metric_code) {
//           case 0:
//             d_xy = x_curr - y_curr;
//             accum += d_xy * d_xy;
//             break;
//           case 1:
//             d_xy = fabs(x_curr - y_curr);
//             if (d_xy > accum) accum = d_xy;
//             break;
//           case 2:
//             d_xy = fabs(x_curr - y_curr);
//             accum += d_xy;
//             break;
//           case 3:
//             d_xy = fabs(x_curr - y_curr);
//             accum += pow(d_xy, param1);
//             break;
//           case 4:
//             x_plus_y = fabs(x_curr) + fabs(y_curr);
//             d_xy = fabs(x_curr - y_curr);
//             if (x_plus_y > 0) accum += d_xy / x_plus_y;
//             break;
//           }
//         } /* while (xi, yi) */
//
//   switch (metric_code) {
//   case 0:
//     dist(col1, col2) = sqrt(accum);
//     break;
//   case 1:
//   case 2:
//   case 4:
//     dist(col1, col2) = accum;
//     break;
//   case 3:
//     if (param1 > 1.0)
//       dist(col1, col2) = pow(accum, 1.0 / param1);
//     else
//       dist(col1, col2) = accum;
//     break;
//   }
//
//       } /* for (col1) */
//     } /* for (col2) */
//
//   if (symmetric) mk_symmetric_matrix(dist);
//   return dist;
// }
//
//
// /*
//  * check OpenMP availability and set desired number of threads
//  */
//
// #ifdef _OPENMP
// #include <omp.h>
// #endif
//
// int openmp_threads = 1;
//
// // [[Rcpp::export]]
// DataFrame CPP_get_openmp_threads() {
//   int num_threads = openmp_threads;
// #ifdef _OPENMP
//   int max_threads = omp_get_max_threads();
// #else
//   int max_threads = 0;
// #endif
//   DataFrame res =
//     DataFrame::create(_["available"] = max_threads > 0,
//                       _["max"] = max_threads,
//                       _["threads"] = num_threads);
//   res.attr("row.names") = "OpenMP";
//   return res;
// }
//
// // [[Rcpp::export]]
// void CPP_set_openmp_threads(int n) {
//   if (n < 1) stop("internal error -- number of threads must be >= 1");
// #ifdef _OPENMP
//   int max_threads = omp_get_max_threads();
//   if (n > max_threads) n = max_threads;
//   openmp_threads = n;
// #else
//   if (n > 1) Rf_warning("OpenMP support not available");
//   openmp_threads = 1;
// #endif
// }
