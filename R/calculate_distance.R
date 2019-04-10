#' Calculate (column-wise) distances/similarity between two matrices
#'
#' These matrices can be dense or sparse.
#'
#' @param x A numeric matrix, dense or sparse.
#' @param y (Optional) a numeric matrix, dense or sparse, with `nrow(x) == nrow(y)`.
#' @param metric Which distance metric to use. Options are: `"cosine"`, `"pearson"`, `"spearman"`, `"euclidean"`, and `"manhattan"`.
#' @param margin Which margin to use for the pairwise comparison. 1 => rowwise, 2 => columnwise.
#'
#' @export
#'
#' @importFrom proxyC dist
#'
#' @examples
#' ## Generate two matrices with 50 and 100 samples
#' library(Matrix)
#' x <- Matrix::rsparsematrix(50, 1000, .01)
#' y <- Matrix::rsparsematrix(100, 1000, .01)
#'
#' dist_euclidean <- calculate_distance(x, y, metric = "euclidean")
#' dist_manhattan <- calculate_distance(x, y, metric = "manhattan")
#' dist_spearman <- calculate_distance(x, y, metric = "spearman")
#' dist_pearson <- calculate_distance(x, y, metric = "pearson")
#' dist_angular <- calculate_distance(x, y, metric = "cosine")
calculate_distance <- function(
  x,
  y = NULL,
  metric = c("cosine", "pearson", "spearman", "euclidean", "manhattan"),
  margin = 1
) {
  metric <- match.arg(metric)
  input <- .process_input_matrices(x = x, y = y, margin = margin)
  x <- input$x
  y <- input$y

  if (metric %in% c("cosine", "pearson", "spearman")) {
    sim <- calculate_similarity(x = x, y = y, metric = metric, margin = 2)
    sim@x[sim@x > 1] <- 1 # due to rounding errors, x can be larger than 1
    if (metric == "cosine") {
      1 - 2 * acos(sim) / pi
    } else {
      1 - (sim + 1) / 2
    }
  } else {
    proxyC::dist(x = x, y = y, method = metric, margin = 2)
  }
}

#' @rdname calculate_distance
#' @export
list_distance_metrics <- function() eval(formals(calculate_distance)$metric)

#' @rdname calculate_distance
#' @export
#' @importFrom proxyC simil
calculate_similarity <- function(
  x,
  y = NULL,
  margin = 1,
  metric = c("cosine", "pearson", "spearman")
) {
  metric <- match.arg(metric)
  input <- .process_input_matrices(x = x, y = y, margin = margin)
  x <- input$x
  y <- input$y

  # run metric
  if (metric %in% c("pearson", "spearman")) {
    if (metric == "spearman") {
      x <- semi_rank(x)
      if (!is.null(y)) {
        y <- semi_rank(y)
      }
    }
    metric <- "correlation"
  }

  proxyC::simil(x = x, y = y, method = metric, margin = 2)
}

#' @rdname calculate_distance
#' @export
list_similarity_metrics <- function() eval(formals(calculate_similarity)$metric)

# this should be turned into rcpp
semi_rank <- function(x) {
  for (i in seq_len(length(x@p) - 1)) {
    pi <- x@p[[i]] + 1
    pj <- x@p[[i + 1]]
    vals <- x@x[pi:pj]
    rvals <- rank(vals)
    nnegs <- sum(vals < 0)
    nposs <- pj - pi - nnegs
    nzeros <- nrow(x) - pj + pi
    new_vals <- ifelse(
      vals < 0,
      rvals - nnegs - nzeros / 2,
      rvals - nnegs - 1 + nzeros / 2
    )
    x@x[pi:pj] <- new_vals
  }
  x
}

#' @importFrom Matrix t
.process_input_matrices <- function(x, y, margin) {
  if (is.data.frame(x)) x <- as.matrix(x)
  if (is.matrix(x)) x <- as(x, "dgCMatrix")

  if (!is.null(y)) {
    if (is.data.frame(y)) y <- as.matrix(y)
    if (is.matrix(y)) y <- as(y, "dgCMatrix")
  }

  assert_that(
    is_sparse(x),
    is.null(y) || is_sparse(y)
  )

  if (margin == 1) {
    if (!is.null(y)) {
      assert_that(ncol(x) == ncol(y))
      y <- Matrix::t(y)
    }
    x <- Matrix::t(x)
  } else {
    if (!is.null(y)) {
      assert_that(nrow(x) == nrow(y))
    }
  }

  list(x = x, y = y)
}

.process_pairwise_matrix <- function(mat, x, y) {
  assert_that(
    ncol(x) == nrow(mat),
    ncol(y) == ncol(mat)
  )
  dimnames(mat) <- list(colnames(x), colnames(y))
  mat
}

