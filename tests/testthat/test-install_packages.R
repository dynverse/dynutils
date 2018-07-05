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

  if (check_packages("whoami")) remove.packages("whoami")
  out <- install_packages("whoami", package = "desc")
  remove.packages("whoami")
  expect_equal(out, "whoami")

  if (check_packages("princurve")) remove.packages("princurve")
  options(dynutils_testmodepromptresponse = 2)
  expect_error(
    expect_message(
      out <- install_packages("princurve", prompt = TRUE),
      "Following packages have to be installed"
    ),
    "Installation was interrupted"
  )

  if (check_packages("princurve")) remove.packages("princurve")
  options(dynutils_testmodepromptresponse = 1)
  expect_message(out <- install_packages("princurve", prompt = TRUE), "Following packages have to be installed")
  remove.packages("princurve")
  expect_equal(out, "princurve")
})

