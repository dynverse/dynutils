#' Rescale data to have a certain center and max range.
#'
#' \code{scale_uniform} uniformily scales a given matrix such that
#' the returned space is centered on \code{center}, and each column was scaled equally
#' such that the range of each column is at most \code{max_range}.
#'
#' @param x A numeric vector matrix or data frame.
#' @param center The new center point of the data.
#' @param max_range The maximum range of each column.
#'
#' @return The centered, scaled matrix. The numeric centering and scalings used are returned as attributes.
#'
#' @export
#'
#' @examples
#' ## Generate a matrix from a normal distribution
#' ## with a large standard deviation, centered at c(5, 5)
#' x <- matrix(rnorm(200*2, sd = 10, mean = 5), ncol = 2)
#'
#' ## Center the dataset at c(0, 0) with a minimum of c(-.5, -.5) and a maximum of c(.5, .5)
#' x_scaled <- scale_uniform(x, center = 0, max_range = 1)
#'
#' ## Plot rescaled data
#' plot(x_scaled)
#'
#' ## Show ranges of each column
#' apply(x_scaled, 2, range)
scale_uniform <- function(x, center = 0, max_range = 1) {
  if (is.null(dim(x))) {
    sc <- scale_uniform(matrix(x, ncol = 1), center = center, max_range = max_range)
    out <- sc[,1]
    names(out) <- names(x)
    attr(out, "addend") <- attr(sc, "addend")
    attr(out, "multiplier") <- attr(sc, "multiplier")
    out
  } else {
    ranges <- apply(x, 2, range)
    cur_range <- max(apply(ranges, 2, diff))

    if (cur_range != 0) {
      multiplier <- max_range / cur_range
    } else {
      multiplier <- 1
    }

    addend <- center / multiplier - colMeans(ranges)

    # calculate rescaled data
    apply_uniform_scale(x, addend, multiplier)
  }
}

#' Apply a uniform scale
#'
#' @param x A numeric vector, matrix or data frame.
#' @param addend A centering vector for each column
#' @param multiplier A scaling vector for each column
#'
#' @return The centered, scaled matrix. The numeric centering and scalings used are returned as attributes.
#' @export
apply_uniform_scale <- function(x, addend, multiplier) {
  if (is.null(dim(x))) {
    sc <- apply_uniform_scale(matrix(x, ncol = 1), addend = addend, multiplier = multiplier)
    out <- sc[,1]
    names(out) <- names(x)
    attr(out, "addend") <- attr(sc, "addend")
    attr(out, "multiplier") <- attr(sc, "multiplier")
    out
  } else {
    y <- x %>%
      sweep(2, addend, "+") %>%
      sweep(2, multiplier, "*")
    attr(y, "addend") <- addend
    attr(y, "multiplier") <- multiplier
    y
  }
}

#' Cut off outer quantiles and rescale to a \[0, 1\] range
#'
#' @param x A numeric vector, matrix or data frame.
#' @param outlier_cutoff The quantile cutoff for outliers (default 0.05).
#'
#' @return The centered, scaled matrix or vector. The numeric centering and scalings used are returned as attributes.
#'
#' @export
#'
#' @importFrom stats quantile
#'
#' @examples
#' ## Generate a matrix from a normal distribution
#' ## with a large standard deviation, centered at c(5, 5)
#' x <- matrix(rnorm(200*2, sd = 10, mean = 5), ncol = 2)
#'
#' ## Scale the dataset between [0,1]
#' x_scaled <- scale_quantile(x)
#'
#' ## Plot rescaled data
#' plot(x_scaled)
#'
#' ## Show ranges of each column
#' apply(x_scaled, 2, range)
scale_quantile <- function(x, outlier_cutoff = .05) {
  if (is.null(dim(x))) {
    sc <- scale_quantile(matrix(x, ncol = 1), outlier_cutoff = outlier_cutoff)
    out <- sc[,1]
    names(out) <- names(x)
    attr(out, "addend") <- attr(sc, "addend")
    attr(out, "multiplier") <- attr(sc, "multiplier")
    out
  } else {
    quants <- apply(x, 2, stats::quantile, c(outlier_cutoff, 1 - outlier_cutoff), na.rm = TRUE)

    addend <- -quants[1,]
    divisor <- apply(quants, 2, diff)
    divisor[divisor == 0] <- 1

    apply_quantile_scale(x, addend, 1 / divisor)
  }
}

#' Apply a quantile scale.
#'
#' Anything outside the range of \[0, 1\] will be set to 0 or 1.
#'
#' @param x A numeric vector, matrix or data frame.
#' @param addend A minimum vector for each column
#' @param multiplier A scaling vector for each column
#'
#' @return The scaled matrix or vector. The numeric centering and scalings used are returned as attributes.
#' @export
apply_quantile_scale <- function(x, addend, multiplier) {
  if (is.null(dim(x))) {
    sc <- apply_quantile_scale(matrix(x, ncol = 1), addend = addend, multiplier = multiplier)
    out <- sc[,1]
    names(out) <- names(x)
    attr(out, "addend") <- attr(sc, "addend")
    attr(out, "multiplier") <- attr(sc, "multiplier")
    out
  } else {
    y <- apply_uniform_scale(x, addend, multiplier)
    y[y > 1] <- 1
    y[y < 0] <- 0
    y
  }
}

#' Rescale data to a \[0, 1\] range
#'
#' @param x A numeric vector, matrix or data frame.
#' @return The centered, scaled matrix or vector. The numeric centering and scalings used are returned as attributes.
#'
#' @export
#'
#' @examples
#' ## Generate a matrix from a normal distribution
#' ## with a large standard deviation, centered at c(5, 5)
#' x <- matrix(rnorm(200*2, sd = 10, mean = 5), ncol = 2)
#'
#' ## Minmax scale the data
#' x_scaled <- scale_minmax(x)
#'
#' ## Plot rescaled data
#' plot(x_scaled)
#'
#' ## Show ranges of each column
#' apply(x_scaled, 2, range)
scale_minmax <- function(x) {
  scale_quantile(x, 0)
}

#' Apply a nubnax scale.
#'
#' Anything outside the range of \[0, 1\] will be set to 0 or 1.
#'
#' @param x A numeric vector, matrix or data frame.
#' @param addend A minimum vector for each column
#' @param multiplier A scaling vector for each column
#'
#' @return The scaled matrix or verctor. The numeric centering and scalings used are returned as attributes.
#' @export
apply_minmax_scale <- apply_quantile_scale

