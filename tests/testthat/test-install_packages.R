context("Testing install_packages")

test_that("Test check_packages", {
  check <- check_packages(c("dplyr", "dynutils", "jhrveioohvovwhrei", "ijewiojwijoweew"))
  expect_equal(check, c(dplyr = TRUE, dynutils = TRUE, jhrveioohvovwhrei = FALSE, ijewiojwijoweew = FALSE))
})

test_that("Test install_packages", {
  # test whether no message is printed when packages are already installed
  expect_message(install_packages("dynutils", "dplyr"), NA)

  # test whether no output is returned when packages are already installed
  out <- install_packages("dynutils", "dplyr")
  expect_null(out)

  # intentionally remove tiny package, see whether it gets reinstalled
  remove.packages("glue")
  out <- install_packages("glue")
  expect_equal(out, "glue")

  # specify package
  remove.packages("desc")
  out <- install_packages("desc", package = "dynutils")
  expect_equal(out, "desc")
})

