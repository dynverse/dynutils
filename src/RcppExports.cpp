// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// project_to_segments
List project_to_segments(NumericMatrix x, NumericMatrix segment_start, NumericMatrix segment_end);
RcppExport SEXP _dynutils_project_to_segments(SEXP xSEXP, SEXP segment_startSEXP, SEXP segment_endSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type segment_start(segment_startSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type segment_end(segment_endSEXP);
    rcpp_result_gen = Rcpp::wrap(project_to_segments(x, segment_start, segment_end));
    return rcpp_result_gen;
END_RCPP
}
// spearman_rank_sparse_rcpp
NumericVector spearman_rank_sparse_rcpp(NumericVector& x, IntegerVector& p, int nrow);
RcppExport SEXP _dynutils_spearman_rank_sparse_rcpp(SEXP xSEXP, SEXP pSEXP, SEXP nrowSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector& >::type p(pSEXP);
    Rcpp::traits::input_parameter< int >::type nrow(nrowSEXP);
    rcpp_result_gen = Rcpp::wrap(spearman_rank_sparse_rcpp(x, p, nrow));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_dynutils_project_to_segments", (DL_FUNC) &_dynutils_project_to_segments, 3},
    {"_dynutils_spearman_rank_sparse_rcpp", (DL_FUNC) &_dynutils_spearman_rank_sparse_rcpp, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_dynutils(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
