check_output <- function(x, y, o, e) {
  expect_equal(nrow(o), nrow(x))
  expect_equal(rownames(o), rownames(x))

  if (is.null(y)) y <- x
  expect_equal(ncol(o), nrow(y))
  expect_equal(colnames(o), rownames(y))

  if (!is.null(e)) {
    expect_equal(as.vector(o), e, tolerance = 1e-10)
  }
}

test_that("list_distance_methods works", {
  expect_true(all(c("euclidean", "manhattan", "cosine", "spearman", "pearson") %in% list_distance_methods()))
})

methods <- list_distance_methods()

test_that("calculate_distance and other functions return the correct format", {
  x <- matrix(c(1, 2, 5, 3), ncol = 2)
  y <- matrix(c(5, 6, 7, 8, 9, 10), ncol = 2)

  for (method in methods) {
    o <- calculate_distance(x, y, method)
    check_output(x, y, o, e = NULL)
  }

  rownames(x) <- c("A", "B")
  rownames(y) <- c("C", "D", "E")
  colnames(x) <- colnames(y) <- c("f1", "f2")

  for (method in methods) {
    o <- calculate_distance(x, y, method)
    check_output(x, y, o, e = NULL)
  }
})

test_that("calculate_distance works when y is NULL", {
  x <- matrix(c(1, 2, 5, 3), ncol = 2)
  y <- NULL

  for (method in methods) {
    o <- calculate_distance(x, y, method)
    check_output(x, y, o, e = NULL)
  }

  rownames(x) <- c("A", "B")
  colnames(x) <- c("f1", "f2")

  for (method in methods) {
    o <- calculate_distance(x, y, method)
    check_output(x, y, o, e = NULL)
  }
})


test_that("calculate_distance returns correct solutions", {
  x <- matrix(c(1, 7, 2, 3), nrow = 1)
  y <- matrix(c(1.5, 4.5, 2.5, 6.5), nrow = 1)
  rx <- t(rank(x))
  ry <- t(rank(y))

  expected <- c(
    euclidean = sqrt(sum((x - y)^2)),
    manhattan = sum(abs(x - y)),
    spearman = 1 - (cov(t(rx), t(ry)) / sd(rx) / sd(ry) + 1) / 2,
    pearson = 1 - (cov(t(x), t(y)) / sd(x) / sd(y) + 1) / 2,
    cosine = 1 - acos(sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))) * 2 / pi
  )

  observed <- sapply(names(expected), function(n) {
    calculate_distance(x, y, method = n)[1,1]
  })

  expect_equal(observed, expected, ignore_attr = TRUE)
})


test_that("calculate_similarity returns correct solutions", {
  x <- matrix(c(1, 7, 2, 3), nrow = 1)
  y <- matrix(c(1.5, 4.5, 2.5, 6.5), nrow = 1)
  rx <- t(rank(x))
  ry <- t(rank(y))

  expected <- c(
    spearman = cov(t(rx), t(ry)) / sd(rx) / sd(ry),
    pearson = cov(t(x), t(y)) / sd(x) / sd(y),
    cosine = sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
  )

  observed <- sapply(names(expected), function(n) {
    calculate_similarity(x, y, method = n)[1,1]
  })

  expect_equal(observed, expected, ignore_attr = TRUE)
})


test_that("calculate_distance works on matrices, data frames and sparse matrices", {
  x <- Matrix::rsparsematrix(10, 100, .01)
  y <- Matrix::rsparsematrix(20, 100, .01)

  for (method in methods) {
    out <- calculate_distance(x = x, y = y, method = method)
    expect_equal(nrow(out), nrow(x))
    expect_equal(ncol(out), nrow(y))

    out2 <- calculate_distance(x = as.matrix(x), y = as.matrix(y), method = method)
    expect_equal(nrow(out2), nrow(x))
    expect_equal(ncol(out2), nrow(y))
    expect_true(sum(abs(na.omit(out2-out))) < 1e-10)

    out3 <- calculate_distance(x = as.data.frame(as.matrix(x)), y = as.data.frame(as.matrix(y)), method = method)
    expect_equal(nrow(out3), nrow(x))
    expect_equal(ncol(out3), nrow(y))
    expect_true(sum(abs(na.omit(out3-out))) < 1e-10)
  }
})


# the same data in every storage format that calculate_distance() and calculate_similarity() accept
as_formats <- function(x) {
  list(
    dense = x,
    data_frame = as.data.frame(x),
    csc = as(x, "CsparseMatrix"),
    csr = as(x, "RsparseMatrix"),
    triplet = as(x, "TsparseMatrix"),
    symmetric = Matrix::forceSymmetric(as(x, "CsparseMatrix"))
  )
}

set.seed(1)
x_sym <- as.matrix(Matrix::forceSymmetric(Matrix::rsparsematrix(8, 8, .5)))
# forceSymmetric() symmetrises dimnames, so use the same row and column names
dimnames(x_sym) <- list(paste0("c", 1:8), paste0("c", 1:8))
y_any <- as.matrix(Matrix::rsparsematrix(8, 8, .5))
dimnames(y_any) <- list(paste0("s", 1:8), paste0("d", 1:8))

test_that("calculate_similarity gives the same result for every storage format and margin", {
  for (margin in c(1, 2)) {
    for (method in list_similarity_methods()) {
      expected <- calculate_similarity(x_sym, y_any, margin = margin, method = method)

      for (fmt in names(as_formats(x_sym))) {
        x <- as_formats(x_sym)[[fmt]]
        y <- as_formats(y_any)[[if (fmt == "symmetric") "csr" else fmt]]
        observed <- calculate_similarity(x, y, margin = margin, method = method)
        expect_equal(observed, expected, tolerance = 1e-10, info = paste(method, fmt, margin))
      }
    }
  }
})

test_that("calculate_similarity computes the correct spearman correlation for every storage format and margin", {
  for (margin in c(1, 2)) {
    xd <- if (margin == 1) t(x_sym) else x_sym
    yd <- if (margin == 1) t(y_any) else y_any
    gold <- suppressWarnings(cor(xd, yd, method = "spearman"))
    gold[is.na(gold)] <- 0

    for (fmt in names(as_formats(x_sym))) {
      x <- as_formats(x_sym)[[fmt]]
      y <- as_formats(y_any)[[if (fmt == "symmetric") "csr" else fmt]]
      observed <- calculate_similarity(x, y, margin = margin, method = "spearman")
      expect_equal(observed, gold, tolerance = 1e-10, info = paste(fmt, margin))
    }
  }
})

test_that("calculate_distance gives the same result for every storage format and margin", {
  for (margin in c(1, 2)) {
    for (method in methods) {
      expected <- calculate_distance(x_sym, y_any, margin = margin, method = method)

      for (fmt in names(as_formats(x_sym))) {
        x <- as_formats(x_sym)[[fmt]]
        y <- as_formats(y_any)[[if (fmt == "symmetric") "csr" else fmt]]
        observed <- calculate_distance(x, y, margin = margin, method = method)
        expect_equal(observed, expected, tolerance = 1e-10, info = paste(method, fmt, margin))
      }
    }
  }
})

test_that(".as_column_vectors reuses the slots of a CSR matrix for margin 1", {
  x <- as(y_any, "RsparseMatrix")
  expect_s4_class(x, "dgRMatrix")
  expect_identical(.as_column_vectors(x, margin = 1), Matrix::t(as(x, "CsparseMatrix")))
  expect_identical(.as_column_vectors(x, margin = 2), as(x, "CsparseMatrix"))
})
