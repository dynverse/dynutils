context("Testing install_packages")

test_that("Test check_packages", {
  check <- check_packages(c("dplyr", "dynutils", "jhrveioohvovwhrei", "ijewiojwijoweew"))
  expect_equal(check, c(dplyr = TRUE, dynutils = TRUE, jhrveioohvovwhrei = FALSE, ijewiojwijoweew = FALSE))
})

r <- getOption("repos")
r["CRAN"] <- "http://cran.r-project.org"
options(repos = r)

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

  options(dynutils_testmodepromptresponse = 2)
  expect_error(out <- install_packages("princurve", prompt = TRUE), "Installation was interrupted")

  options(dynutils_testmodepromptresponse = 1)
  expect_message(out <- install_packages("princurve", prompt = TRUE), "Following packages have to be installed")
  on.exit(remove.packages("princurve"))
  expect_equal(out, "princurve")
})

