#' Scale data on a given center and between a given range
#'
#' @description \code{scale_uniformily} uniformily scales a given matrix such
#' that the returned space is centered on \code{center}, and each column was
#' scaled equally such that the range of each column is at most \code{max_range}.
#'
#' @usage
#' scale_uniformily(x, center =  0, max_range = 1)
#'
#' @param x A numeric matrix or data frame.
#' @param center The new center point of the data.
#' @param max_range The maximum range of each column.
#'
#' @return The centered and scaled matrix. The numeric centering and scalings used are returned as attributes.
#'
#' @export
scale_uniformily <- function(x, center = 0, max_range = 1) {
  ranges <- apply(x, 2, range)

  new_scale <- max(ranges[2,] - ranges[1,]) / max_range
  new_center <- (ranges[1,] + ranges[2,] - center * new_scale) / 2

  # calculate rescaled data
  apply_uniform_scale(x, new_center, new_scale)
}

#' Apply a uniform scale
#'
#' @param x A numeric matrix or data frame.
#' @param center A centering vector for each column
#' @param scale A scaling vector for each column
#'
#' @return The centered and scaled matrix. The numeric centering and scalings used are returned as attributes.
#' @export
apply_uniform_scale <- function(x, center, scale) {
  y <- t(apply(x, 1, function(x) (x - center) / scale))
  attr(y, "center") <- center
  attr(y, "scale") <- scale
  y
}
