context("Testing install_packages")

test_that("Test check_packages", {
  check <- check_packages(c("dplyr", "dynutils", "jhrveioohvovwhrei", "ijewiojwijoweew"))
  expect_equal(check, c(dplyr = TRUE, dynutils = TRUE, jhrveioohvovwhrei = FALSE, ijewiojwijoweew = FALSE))
})

test_that("Test install_packages", {
  # test whether no message is printed when packages are already installed
  expect_message(out <- install_packages(c("dynutils", "dplyr")), NA)

  # test whether no output is returned when packages are already installed
  expect_null(out)

  # add depending package
  out <- install_packages("dplyr", dependencies = "dynutils")
  expect_null(out)

  # test with a small package
  out <- install_packages("incgraph")
  expect_equal(out, "incgraph")

  # the rest of this test does not work with covr, for some reason
  skip_on_travis()

  # intentionally remove tiny package, see whether it gets reinstalled
  remove.packages("glue")
  out <- install_packages("glue")
  expect_equal(out, "glue")

  # specify depending package
  remove.packages("desc")
  out <- install_packages("desc", package = "dynutils")
  expect_equal(out, "desc")
})

