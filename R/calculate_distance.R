#' @importFrom testthat expect_is
calculate_distance_preproc_x <- function(x) {
  assert_that(
    is.matrix(x) || is.data.frame(x) || grepl("...Matrix", class(x)[[1]])
  )
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  }
  x
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

#' @rdname calculate_distance
#'
#' @inheritParams stats::cor
#'
#' @importFrom stats cor
#' @importFrom Matrix t
#'
#' @include inherit_default_params.R
#'
#' @export
correlation_distance <- inherit_default_params(
  list(stats::cor),
  function(x, y, method, use) {
    x <- calculate_distance_preproc_x(x)
    y <- calculate_distance_preproc_y(x, y)

    sim <- stats::cor(Matrix::t(x), Matrix::t(y), method = method, use = use)
    d <- 1 - (sim + 1) / 2

    calculate_distance_postproc_d(x, y, d)
  }
)

spearman_distance = inherit_default_params(
  list(correlation_distance),
  function(x, y, use) {
    correlation_distance(x = x, y = y, method = "spearman", use = use)
  }
)

pearson_distance = inherit_default_params(
  list(correlation_distance),
  function(x, y, use) {
    correlation_distance(x = x, y = y, method = "pearson", use = use)
  }
)

kendall_distance = inherit_default_params(
  list(correlation_distance),
  function(x, y, use) {
    correlation_distance(x = x, y = y, method = "kendall", use = use)
  }
)

#' @rdname calculate_distance
#'
#' @include inherit_default_params.R
#'
#' @importFrom Matrix t rowSums
#' @export
angular_distance <- function(x, y = NULL) {
  x <- calculate_distance_preproc_x(x)
  y <- calculate_distance_preproc_y(x, y)

  top <- x %*% Matrix::t(y)
  bot1 <- sqrt(Matrix::rowSums(x^2))
  bot2 <- sqrt(Matrix::rowSums(y^2))

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

  # borrow from https://github.com/cran/wordspace/blob/master/src/wordspace.cpp
  if (grepl("...Matrix", class(x)[[1]]) || grepl("...Matrix", class(y)[[1]])) {
    d <- matrix(NA, nrow = nrow(x), ncol = nrow(y))
    # for (i in seq_len(nrow(x))) {
    #   for (j in seq_len(nrow(y))) {
    #     d[i, j] <- sum(abs(x[i, ] - y[j, ]))
    #   }
    # }
  } else {
    d <- .Call('_dynutils_manhattan_distance', PACKAGE = 'dynutils', x, y)
  }

  calculate_distance_postproc_d(x, y, d)
}

#' @rdname calculate_distance
#'
#' @export
euclidean_distance <- function(x, y = NULL) {
  x <- calculate_distance_preproc_x(x)
  y <- calculate_distance_preproc_y(x, y)

  # borrow from https://github.com/cran/wordspace/blob/master/src/wordspace.cpp
  if (grepl("...Matrix", class(x)[[1]]) || grepl("...Matrix", class(y)[[1]])) {
    d <- matrix(NA, nrow = nrow(x), ncol = nrow(y))
    # for (i in seq_len(nrow(x))) {
    #   for (j in seq_len(nrow(y))) {
    #     d[i, j] <- sum(abs(x[i, ] - y[j, ])^2)
    #   }
    # }
  } else {
    d <- .Call('_dynutils_euclidean_distance', PACKAGE = 'dynutils', x, y)
  }

  calculate_distance_postproc_d(x, y, d)
}

#' @rdname calculate_distance
#'
#' @export
list_distance_metrics <- function() {
  list(
    euclidean = euclidean_distance,
    manhattan = manhattan_distance,
    angular = angular_distance,
    spearman = spearman_distance,
    pearson = pearson_distance,
    kendall = kendall_distance
  )
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
  method = names(list_distance_metrics())
) {
  method <- match.arg(method)

  fun <- list_distance_metrics()[[method]]
  fun(x = x, y = y)
}

# evaluate names of distance metrics
formals(calculate_distance)$method <- names(list_distance_metrics())
