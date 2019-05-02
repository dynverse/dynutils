spearman_rank_sparse <- function(x) {
  x@x <- spearman_rank_sparse_rcpp(x@x, x@p, nrow(x))
  x
}
