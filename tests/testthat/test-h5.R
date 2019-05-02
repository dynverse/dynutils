context("Testing write_h5 and read_h5")

obj <- get_h5_test_data()

test_that("write_h5 and read_h5 works", {
  file <- tempfile()
  on.exit(file.remove(file))

  write_h5(obj, file)

  obj2 <- read_h5(file)

  testthat::expect_equivalent(obj2, obj)
})

test_that("test_h5_installation works", {
  expect_true(test_h5_installation())
  expect_message(test_h5_installation(detailed = TRUE), "HDF5 test successful")

  expect_output(expect_error(test_h5_installation_write(detailed = TRUE, obj = list(x = print))))
  expect_output(expect_error(test_h5_installation_read(detailed = TRUE, file = tempfile())))
  expect_output(expect_error(test_h5_installation_equal(detailed = TRUE, obj = 1, obj2 = 2)))
})

test_that("is_sparse works", {
  expect_false(is_sparse(matrix(c(1:10))))

  m <- Matrix::Matrix(matrix(c(1:10)), sparse = FALSE)
  expect_false(is_sparse(m))
  expect_true(is_sparse(methods::as(m, "dgCMatrix")))
  expect_false(is_sparse(methods::as(m, "dgeMatrix")))
})


test_that("errors gracefully", {
  file <- tempfile()
  on.exit(file.remove(file))

  h5file <- hdf5r::H5File$new(file, mode = "w")
  h5file[["a"]] <- 1
  h5file$close_all()

  expect_error(read_h5(file), regexp = "Object class not found")
})

