#' Check if an object is a sparse matrix
#'
#' @param x An object to test
#'
#' @export
#'
#' @importFrom methods is
#'
#' @examples
#' is_sparse(matrix(1:10)) # FALSE
#' is_sparse(Matrix::rsparsematrix(100, 200, .01)) # TRUE
is_sparse <- function(x) {
  is(x, "sparseMatrix")
}
