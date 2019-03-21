#' Calculate a (weighted) mean between vectors or a list of vectors
#'
#' This function supports the arithmetic, geometric and harmonic mean.
#'
#' @param ... Can be:
#'   - One numeric vector
#'   - A list containg numeric vectors
#'   - Numeric vectors given as separate inputs
#'
#' @param method The aggregation function. Must be one of `"arithmetic"`, `"geometric"`, and `"harmonic"`.
#' @param weights Weights with the same length as `...`.
#'
#' @export
#'
#' @examples
#' calculate_arithmetic_mean(0.1, 0.5, 0.9)
#' calculate_geometric_mean(0.1, 0.5, 0.9)
#' calculate_harmonic_mean(0.1, 0.5, 0.9)
#' calculate_mean(.1, .5, .9, method = "harmonic")
#'
#' # example with multiple vectors
#' calculate_arithmetic_mean(c(0.1, 0.9), c(0.2, 1))
#'
#' # example with a list of vectors
#' vectors <- list(c(0.1, 0.2), c(0.4, 0.5))
#' calculate_geometric_mean(vectors)
#'
#' # example of weighted means
#' calculate_geometric_mean(c(0.1, 10), c(0.9, 20), c(0.5, 2), weights = c(1, 2, 5))
calculate_mean <- function(..., method, weights = NULL) {
  fun <-
    list(
      arithmetic = calculate_arithmetic_mean,
      harmonic = calculate_harmonic_mean,
      geometric = calculate_geometric_mean
    )[[method]]
  fun(..., weights = weights)
}

#' @rdname calculate_mean
#' @export
calculate_harmonic_mean <- function(..., weights = NULL) {
  x <- process_combination_input(...)
  if (is.null(weights)) {
    ncol(x) / rowSums(1/x)
  } else {
   sum(weights) / rowSums(process_weights(weights, nrow(x))/x)
  }
}

#' @rdname calculate_mean
#' @export
calculate_geometric_mean <- function(..., weights = NULL) {
  x <- process_combination_input(...)
  if (is.null(weights)) {
    apply(x, 1, prod)^(1/ncol(x))
  } else {
    exp(rowSums(process_weights(weights, nrow(x)) * log(x)) / sum(weights))
  }
}

#' @rdname calculate_mean
#' @export
calculate_arithmetic_mean <- function(..., weights = NULL) {
  x <- process_combination_input(...)

  if (is.null(weights)) {
    rowSums(x)/ncol(x)
  } else {
    rowSums(x * process_weights(weights, nrow(x))) / sum(weights)
  }
}

# Processes:
#   - process_combination_input(list(1, 2, 3))
#   - process_combination_input(c(1, 2, 3))
#   - process_combination_input(1, 2, 3)
# all to matrix(c(1, 2, 3))
process_combination_input <- function(...) {
  dots <- list(...)
  if (length(dots) > 1 && all(map_lgl(dots, is.numeric))) {
    do.call(cbind, dots)
  } else if (is.list(..1) && all(map_lgl(..1, is.numeric))) {
    do.call(cbind, ..1)
  } else if (is.matrix(..1) && is.numeric(..1)) {
    ..1
  } else if (is.numeric(..1)) {
    do.call(cbind, as.list(..1))
  } else {
    stop("Invalid input")
  }
}

process_weights <- function(weights, n_observations) {
  matrix(rep(weights, n_observations), nrow = n_observations, ncol = length(weights), byrow = TRUE)
}
