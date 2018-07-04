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

  are_installed <- check_packages(c("SCORPIUS", "knitr", "glue", "desc"))
  on.exit(remove.packages(names(are_installed)[!are_installed]))

  if (are_installed["SCORPIUS"]) remove.packages("SCORPIUS")
  out <- install_packages("SCORPIUS")
  expect_equal(out, "SCORPIUS")

  if (are_installed["knitr"]) remove.packages("knitr")
  out <- install_packages("knitr", package = "SCORPIUS")
  expect_equal(out, "knitr")

  if (are_installed["glue"]) remove.packages("glue")
  out <- install_packages("glue")
  expect_equal(out, "glue")

  if (are_installed["desc"]) remove.packages("desc")
  out <- install_packages("desc", package = "dynutils")
  expect_equal(out, "desc")


})

