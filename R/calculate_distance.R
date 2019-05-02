#' Calculate (column-wise) distances/similarity between two matrices
#'
#' These matrices can be dense or sparse.
#'
#' @param x A numeric matrix, dense or sparse.
#' @param y (Optional) a numeric matrix, dense or sparse, with `nrow(x) == nrow(y)`.
#' @param method Which distance method to use. Options are: `"cosine"`, `"pearson"`, `"spearman"`, `"euclidean"`, and `"manhattan"`.
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
#' dist_euclidean <- calculate_distance(x, y, method = "euclidean")
#' dist_manhattan <- calculate_distance(x, y, method = "manhattan")
#' dist_spearman <- calculate_distance(x, y, method = "spearman")
#' dist_pearson <- calculate_distance(x, y, method = "pearson")
#' dist_angular <- calculate_distance(x, y, method = "cosine")
calculate_distance <- function(
  x,
  y = NULL,
  method = c("pearson", "spearman", "cosine", "euclidean", "manhattan"),
  margin = 1
) {
  method <- match.arg(method)
  input <- .process_input_matrices(x = x, y = y, margin = margin)
  x <- input$x
  y <- input$y

  dis <-
    if (method %in% c("cosine", "pearson", "spearman")) {
      sim <- calculate_similarity(x = x, y = y, method = method, margin = 2)

      if (method == "cosine") {
        1 - 2 * acos(sim) / pi
      } else {
        1 - (sim + 1) / 2
      }
    } else {
      proxyC::dist(x = x, y = y, method = method, margin = 2)
    }

  if (is.null(y)) {
    diag(dis) <- 0
  }

  as.matrix(dis)
}

#' @rdname calculate_distance
#' @export
list_distance_methods <- function() eval(formals(calculate_distance)$method)

#' @rdname calculate_distance
#' @export
#' @importFrom proxyC simil
calculate_similarity <- function(
  x,
  y = NULL,
  margin = 1,
  method = c("spearman", "pearson", "cosine")
) {
  method <- match.arg(method)
  input <- .process_input_matrices(x = x, y = y, margin = margin)
  x <- input$x
  y <- input$y

  # run method
  if (method %in% c("pearson", "spearman")) {
    if (method == "spearman") {
      x <- spearman_rank_sparse(x)
      if (!is.null(y)) {
        y <- spearman_rank_sparse(y)
      }
    }
    method <- "correlation"
  }

  sim <- proxyC::simil(x = x, y = y, method = method, margin = 2)

  # fixes due to rounding errors
  if (method %in% c("pearson", "spearman", "cosine")) {
    sim@x[sim@x > 1] <- 1

    if (is.null(y)) {
      diag(sim) <- 1
    }
  }

  as.matrix(sim)
}

#' @rdname calculate_distance
#' @export
list_similarity_methods <- function() eval(formals(calculate_similarity)$method)

#' @importFrom Matrix t
#' @importFrom methods as
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


#' These functions will be removed soon
#'
#' Use [calculate_distance()] instead.
#'
#' @inheritParams calculate_distance
#' @export
#' @rdname deprecated
euclidean_distance <- function(x, y = NULL) {
  as.matrix(calculate_distance(x, y, method = "euclidean"))
}

#' @export
#' @rdname deprecated
correlation_distance <- function(x, y = NULL) {
  as.matrix(calculate_distance(x, y, method = "spearman"))
}
