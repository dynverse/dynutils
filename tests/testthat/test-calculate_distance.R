context("Testing calculate_distance")

check_output <- function(x, y, o, e) {
  expect_equal(nrow(o), nrow(x))
  expect_equal(rownames(o), rownames(x))

  if (is.null(y)) y <- x
  expect_equal(ncol(o), nrow(y))
  expect_equal(colnames(o), rownames(y))

  if (!is.null(e)) {
    expect_true(all(abs(as.vector(o) - e) < 1e-10))
  }
}

test_that("list_distance_methods works", {
  expect_true(all(list_distance_methods() %in% c("euclidean", "manhattan", "cosine", "spearman", "pearson")))
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

  expect_equivalent(observed, expected)
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

  expect_equivalent(observed, expected)
})


test_that("calculate_distance works on matrices, data frames and sparse matrices", {
  x <- Matrix::rsparsematrix(100, 10000, .01)
  y <- Matrix::rsparsematrix(2000, 10000, .01)

  for (method in methods) {
    out <- calculate_distance(x = x, y = y, method = method)
    expect_equal(nrow(out), nrow(x))
    expect_equal(ncol(out), nrow(y))

    out2 <- calculate_distance(x = as.matrix(x), y = as.matrix(y), method = method)
    expect_equal(nrow(out2), nrow(x))
    expect_equal(ncol(out2), nrow(y))
    expect_true(sum(abs(out2-out)) < 1e-10)

    out3 <- calculate_distance(x = as.data.frame(as.matrix(x)), y = as.data.frame(as.matrix(y)), method = method)
    expect_equal(nrow(out3), nrow(x))
    expect_equal(ncol(out3), nrow(y))
    expect_true(sum(abs(out3-out)) < 1e-10)
  }
})
