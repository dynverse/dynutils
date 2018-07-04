context("Testing install_packages")

test_that("Test check_packages", {
  check <- check_packages(c("dplyr", "dynutils", "jhrveioohvovwhrei", "ijewiojwijoweew"))
  expect_equal(check, c(dplyr = TRUE, dynutils = TRUE, jhrveioohvovwhrei = FALSE, ijewiojwijoweew = FALSE))
})

options(repos = "http://cran.us.r-project.org")

skip_on_cran()

test_that("Test install_packages", {
  # test whether no message is printed when packages are already installed
  expect_message(out <- install_packages(c("dynutils", "dplyr")), NA)

  # test whether no output is returned when packages are already installed
  expect_null(out)

  # add depending package
  out <- install_packages("dplyr", package = "dynutils")
  expect_null(out)

  out <- install_packages("whoami", package = "desc")
  on.exit(remove.packages("whoami"))
  expect_equal(out, "whoami")
})

