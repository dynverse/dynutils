context("Testing install_packages")

r <- getOption("repos")
r["CRAN"] <- "http://cran.r-project.org"
options(repos = r)

skip_on_cran()

test_that("no messages are printed when packages are already installed", {
  expect_message(out <- install_packages(c("dynutils", "dplyr")), NA)
  expect_null(out)
})

test_that("no messages are printed when packages are already installed, given a depending package", {
  expect_message(out <- install_packages("dplyr", package = "dynutils"), NA)
  expect_null(out)
})

test_that("dependencies can be installed", {
  is_present <- check_packages("whoami")
  if (is_present) remove.packages("whoami")
  out <- install_packages("whoami", is_interactive = FALSE)
  if (!is_present) remove.packages("whoami")
  expect_equal(out, "whoami")
})

test_that("prompt works as expected", {
  is_present <- check_packages("princurve")
  if (is_present) remove.packages("princurve")
  options(dynutils_testmodepromptresponse = 2)
  expect_error(
    expect_message(
      out <- install_packages("princurve", is_interactive = TRUE),
      "Following packages have to be installed"
    ),
    "Installation was interrupted"
  )

  if (check_packages("princurve")) remove.packages("princurve")
  options(dynutils_testmodepromptresponse = 1)
  expect_message(out <- install_packages("princurve", is_interactive = TRUE), "Following packages have to be installed")
  if (is_present) remove.packages("princurve")
  expect_equal(out, "princurve")

  options(dynutils_testmodepromptresponse = NULL)
})

