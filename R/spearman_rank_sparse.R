spearman_rank_sparse <- function(x) {
  x <- .as_column_vectors(x, margin = 2)
  x@x <- spearman_rank_sparse_rcpp(x@x, x@p, nrow(x))
  x
}
