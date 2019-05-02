context("Testing spearman_rank_sparse()")

r_control_implementation <- function(x, p, nr) {
  y <- x
  for (i in seq_len(length(p) - 1)) {
    pi <- p[[i]] + 1
    pj <- p[[i + 1]]
    if (pj >= pi) {
      sz <- pj - pi + 1
      vals <- x[pi:pj]

      nposs <- sum(vals > 0)
      nnegs <- sz - nposs
      nzeros <- nr - sz

      rvals <- rank(vals)
      new_vals <- ifelse(
        vals < 0,
        rvals - nnegs - (nzeros + 1) / 2,
        rvals - nnegs - 1 + (nzeros + 1) / 2
      )
      y[pi:pj] <- new_vals
    }
  }
  y
}

set_nas_to_two <- function(x) {
  x[is.na(x)] <- 2
  x
}

test_that("spearman_rank_sparse works as intended with small example", {
  i <- c(1L:2L, 1L:4L, 1L:3L, 1L)
  x <- c(
    -1, 1,
    3, -2, 6, -1,

    -1, -5, -2,
    5
  )
  p <- c(0L, 2L, 6L, 6L, 9L, 10L)
  nr <- 100L

  out1 <- spearman_rank_sparse_rcpp(x, p, nr)

  out2 <- r_control_implementation(x, p, nr)

  expect_equivalent(out1, out2)
})

test_that("spearman_rank_sparse works as intended with another example", {
  x <- Matrix::rsparsematrix(10, 10, .3)
  gold <- cor(as.matrix(x), method = "spearman") %>% set_nas_to_two()

  x2 <- x
  x2@x <- r_control_implementation(x@x, x@p, x@Dim[[1]])
  r_imp <- cor(as.matrix(x2), method = "pearson") %>% set_nas_to_two()
  expect_true(sum(abs(gold - r_imp)) < 1e-8)

  x3 <- x
  x3@x <- spearman_rank_sparse_rcpp(x@x, x@p, x@Dim[[1]])
  rcpp_imp <- cor(as.matrix(x3), method = "pearson") %>% set_nas_to_two()
  expect_true(sum(abs(gold - rcpp_imp)) < 1e-8)

  x4 <- spearman_rank_sparse(x)
  rcpp_imp2 <- cor(as.matrix(x4), method = "pearson") %>% set_nas_to_two()
  expect_true(sum(abs(gold - rcpp_imp2)) < 1e-8)
})


test_that("spearman_rank_sparse works as intended with larger example", {
  x <- Matrix::rsparsematrix(101, 99, .3)
  gold <- cor(as.matrix(x), method = "spearman") %>% set_nas_to_two()

  x2 <- x
  x2@x <- r_control_implementation(x@x, x@p, x@Dim[[1]])
  r_imp <- cor(as.matrix(x2), method = "pearson") %>% set_nas_to_two()
  expect_true(sum(abs(gold - r_imp)) < 1e-8)

  x3 <- x
  x3@x <- spearman_rank_sparse_rcpp(x@x, x@p, x@Dim[[1]])
  rcpp_imp <- cor(as.matrix(x3), method = "pearson") %>% set_nas_to_two()
  expect_true(sum(abs(gold - rcpp_imp)) < 1e-8)

  x4 <- spearman_rank_sparse(x)
  rcpp_imp2 <- cor(as.matrix(x4), method = "pearson") %>% set_nas_to_two()
  expect_true(sum(abs(gold - rcpp_imp2)) < 1e-8)
})




test_that("spearman_rank_sparse works with only positive integers", {
  x <- Matrix::rsparsematrix(100, 99, .1) %>% abs()
  gold <- cor(as.matrix(x), method = "spearman") %>% set_nas_to_two()

  x2 <- x
  x2@x <- r_control_implementation(x@x, x@p, x@Dim[[1]])
  r_imp <- cor(as.matrix(x2), method = "pearson") %>% set_nas_to_two()
  expect_true(sum(abs(gold - r_imp)) < 1e-8)

  x3 <- x
  x3@x <- spearman_rank_sparse_rcpp(x@x, x@p, x@Dim[[1]])
  rcpp_imp <- cor(as.matrix(x3), method = "pearson") %>% set_nas_to_two()
  expect_true(sum(abs(gold - rcpp_imp)) < 1e-8)

  x4 <- spearman_rank_sparse(x)
  rcpp_imp2 <- cor(as.matrix(x4), method = "pearson") %>% set_nas_to_two()
  expect_true(sum(abs(gold - rcpp_imp2)) < 1e-8)
})



