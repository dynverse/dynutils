#' @importFrom testthat expect_is
calculate_distance_preproc_x <- function(x) {
  testthat::expect_is(x, c("matrix", "data.frame", "dgCMatrix"))
  as.matrix(x)
}

#' @importFrom testthat expect_equal
calculate_distance_preproc_y <- function(x, y) {
  if (is.null(y)) y <- x
  testthat::expect_equal(ncol(y), ncol(x))
  calculate_distance_preproc_x(y)
}

calculate_distance_postproc_d <- function(x, y, d) {
  dimnames(d) <- list(rownames(x), rownames(y))
  d
}

#' Distance metrics
#'
#' Calculate (pairwise) distances between two matrices
#'
#' @param x A numeric matrix
#' @param y (Optional) a numeric matrix, with \code{ncol(x) == ncol(y)}.
#' @param method Distance method to use. Options are:
#' \itemize{
#'   \item euclidean: \code{\link{euclidean_distance}}
#'   \item manhattan: \code{\link{manhattan_distance}}
#'   \item spearman, pearson, or kendall: \code{\link{correlation_distance}}
#'   \item angular: \code{\link{angular_distance}}
#' }
#'
#' @rdname calculate_distance
#'
#' @export
#'
#' @examples
#' ## Generate two matrices with 50 and 100 samples
#' x <- matrix(rnorm(50*10, mean = 0, sd = 1), ncol = 10)
#' y <- matrix(rnorm(100*10, mean = 1, sd = 2), ncol = 10)
#'
#' dist_euclidean <- calculate_distance(x, y, method = "euclidean")
#' dist_manhattan <- calculate_distance(x, y, method = "manhattan")
#' dist_spearman <- calculate_distance(x, y, method = "spearman")
#' dist_pearson <- calculate_distance(x, y, method = "pearson")
#' dist_kendall <- calculate_distance(x, y, method = "kendall")
#' dist_angular <- calculate_distance(x, y, method = "angular")
calculate_distance <- function(
  x,
  y = NULL,
  method = c("euclidean", "manhattan", "spearman", "pearson", "kendall", "angular")
) {
  method <- match.arg(method)

  if (method == "euclidean") {
    euclidean_distance(x, y)
  } else if (method == "manhattan") {
    manhattan_distance(x, y)
  } else if (method %in% c("spearman", "pearson", "kendall")) {
    correlation_distance(x, y, method = method)
  } else if (method == "angular") {
    angular_distance(x, y)
  }
}

#' @rdname calculate_distance
#'
#' @inheritParams stats::cor
#'
#' @importFrom stats cor
#'
#' @include inherit_default_params.R
#'
#' @export
correlation_distance <- inherit_default_params(
  list(stats::cor),
  function(x, y, method, use) {
    x <- calculate_distance_preproc_x(x)
    y <- calculate_distance_preproc_y(x, y)

    d <- 1 - (stats::cor(t(x), t(y), method = method, use = use) + 1) / 2

    calculate_distance_postproc_d(x, y, d)
  }
)

#' @rdname calculate_distance
#'
#' @include inherit_default_params.R
#'
#' @export
angular_distance <- function(x, y = NULL) {
  x <- calculate_distance_preproc_x(x)
  y <- calculate_distance_preproc_y(x, y)

  top <- x %*% t(y)
  bot1 <- sqrt(rowSums(x^2))
  bot2 <- sqrt(rowSums(y^2))

  # optimisation of:
  # div <- top %>% sweep(1, bot1, "/") %>% sweep(2, bot2, "/")
  # https://stackoverflow.com/a/20596490/585801
  bot1 <-
    if (length(bot1) != 1) {
      diag(1 / bot1)
    } else {
      1 / bot1
    }
  bot2 <-
    if (length(bot2) != 1) {
      diag(1 / bot2)
    } else {
      1 / bot2
    }
  div <- bot1 %*% top %*% bot2

  # div should never be larger than 1, but sometimes is due to rounding errors
  div[div > 1] <- 1

  d <- acos(div) * 2 / pi

  calculate_distance_postproc_d(x, y, d)
}

#' @rdname calculate_distance
#'
#' @export
manhattan_distance <- function(x, y = NULL) {
  x <- calculate_distance_preproc_x(x)
  y <- calculate_distance_preproc_y(x, y)

  d <- .Call('_dynutils_manhattan_distance', PACKAGE = 'dynutils', x, y)

  calculate_distance_postproc_d(x, y, d)
}

#' @rdname calculate_distance
#'
#' @export
euclidean_distance <- function(x, y = NULL) {
  x <- calculate_distance_preproc_x(x)
  y <- calculate_distance_preproc_y(x, y)

  d <- .Call('_dynutils_euclidean_distance', PACKAGE = 'dynutils', x, y)

  calculate_distance_postproc_d(x, y, d)
}


