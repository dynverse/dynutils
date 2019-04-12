#include <Rcpp.h>
using namespace Rcpp;

class Comparator {
private:
  const Rcpp::NumericVector& ref;

  bool is_na(double x) const
  {
    return Rcpp::traits::is_na<REALSXP>(x);
  }

public:
  Comparator(const Rcpp::NumericVector& ref_) :
    ref(ref_)
  {}

  bool operator()(const int ilhs, const int irhs) const
  {
    double lhs = ref[ilhs], rhs = ref[irhs];
    if (is_na(lhs)) return false;
    if (is_na(rhs)) return true;
    return lhs < rhs;
  }
};

// Transform  a sparse matrix into a rank sparse matrix,
// right before calculating the spearman rank correlation.
// [[Rcpp::export]]
NumericVector spearman_rank_sparse_rcpp(NumericVector & x, IntegerVector & p, int nrow) {
  NumericVector r = no_init_vector(x.size());

  int pi, pj, sz;
  int num_zero, num_neg;
  double rankval;
  int n;

  for(int i = 0; i < p.length() - 1; i++) {
    pi = p[i];
    pj = p[i + 1] - 1;

    if (pj >= pi) {
      sz = pj - pi + 1;

      num_zero = nrow - sz;
      num_neg = 0;
      for (int j = pi; j <= pj; j++) {
        if (x[j] < 0.0) {
          num_neg++;
        }
      }

      IntegerVector ord = seq(pi, pj);
      std::sort(ord.begin(), ord.end(), Comparator(x));

      for (R_xlen_t j = 0; j < sz; j += n) {
        n = 1;

        while (j + n < sz && x[ord[j]] == x[ord[j + n]]) ++n;

        if (x[ord[j]] > 0.0) {
          rankval = j - num_neg + (n + num_zero) / 2.0;
        } else {
          rankval = j - num_neg + (n - num_zero) / 2.0;
        }

        for (R_xlen_t k = 0; k < n; k++) {
          r[ord[j + k]] = rankval;
        }
      }
    }

  }
  return(r);
}

