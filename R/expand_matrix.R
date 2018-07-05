#' Expand a matrix with given rownames and colnames
#'
#' @param mat The matrix to expand
#' @param rownames The desired rownames
#' @param colnames The desired colnames
#' @param fill With what to fill missing data
#'
#' @export
#'
#' @examples
#' x <- matrix(runif(12), ncol = 4, dimnames = list(c("a", "c", "d"), c("D", "F", "H", "I")))
#' expand_matrix(x, letters[1:5], LETTERS[1:10], fill = 0)
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
