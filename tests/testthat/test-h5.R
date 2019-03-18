context("Testing write_h5 and read_h5")

m <- matrix(1:20, ncol = 4, dimnames = list(letters[1:5], LETTERS[1:4]))

obj <-
  list(
    charone = "a",
    charmany = c("one", "two", "three"),
    logicalone = TRUE,
    even = c(one = FALSE, two = TRUE, three = FALSE),
    listone = list(a = 1, b = 2),
    listtwo = list(mat = matrix(1:10, ncol = 2), df = data.frame(a = 1, b = c(1, 2)), null = NULL),
    listmany = list(list(list())),
    df = data.frame(a = letters[1:4], b = runif(4), c = c(T, F, T, T), d = 2L:5L),
    mat = m,
    spmat = Matrix::Matrix(m, sparse = TRUE),
    null = NULL
  )
class(obj) <- "tenten"

test_that("write_h5 and read_h5 works", {
  file <- tempfile()
  on.exit(file.remove(file))

  write_h5(obj, file)

  obj2 <- read_h5(file)

  testthat::expect_equivalent(obj2, obj)
})


test_that("is_sparse works", {
  expect_false(is_sparse(matrix(c(1:10))))

  m <- Matrix::Matrix(matrix(c(1:10)))
  expect_true(is_sparse(m))
  expect_true(is_sparse(as(m, "dgCMatrix")))
  expect_true(is_sparse(as(m, "dgeMatrix")))
})



