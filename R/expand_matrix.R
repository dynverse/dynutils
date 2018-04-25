#' Expand a matrix with given rownames and colnames
#'
#' @param mat The matrix to expand
#' @param rownames The desired rownames
#' @param colnames The desired colnames
#' @param fill With what to fill missing data
#'
#' @export
expand_matrix <- function(
  mat,
  rownames = NULL,
  colnames = NULL,
  fill = 0
) {
  if (is.null(rownames)) {
    rownames <- rownames(mat)
  }
  if (is.null(colnames)) {
    colnames <- colnames(mat)
  }
  newmat <- matrix(
    fill,
    nrow = length(rownames),
    ncol = length(colnames),
    dimnames = list(rownames, colnames)
  )
  newmat[rownames(mat),colnames(mat)] <- mat
  newmat
}
