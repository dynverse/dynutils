#' Check if an object is a sparse matrix
#'
#' @param x An object to test
#'
#' @export
#'
#' @examples
#' is_sparse(matrix(1:10)) # FALSE
#' is_sparse(Matrix::sparseMatrix(1, 1, 1)) # TRUE
is_sparse <- function(x) {
  is(x, "sparseMatrix")
}
