#' Replace missing values
#'
#' This function is similar to [rlang::%|%] but expects the replacement values
#' to be of equal length to the initial vector.
#'
#' @param x Original values, possibly containing `NA` values.
#' @param y Replacement values, length must be 1 or same as `x`.
#'
#' @export
`%ifna%` <- function(x, y) {
  ifelse(is.na(x), y, x)
}
