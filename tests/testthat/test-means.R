context("Means")

test_that(paste0("calculate_means"), {
  testthat::expect_equal(calculate_arithmetic_mean(0.5, 0.6, 0.7), 0.6)
  testthat::expect_equal(calculate_harmonic_mean(0.5, 1, 1), 0.75)
  testthat::expect_equal(calculate_geometric_mean(0.001, 1, 1), 0.1)

  testthat::expect_equal(calculate_arithmetic_mean(0.1, 0.4, 0.4, weights = c(1, 2, 5)), 0.3625)
  testthat::expect_equal(calculate_harmonic_mean(0.5, 1, 1, weights = c(1, 2, 4)), 0.875)
  testthat::expect_equal(calculate_geometric_mean(0.001, 1, 1, weights = c(1, 2, 0)), 0.1)

  testthat::expect_true(length(calculate_arithmetic_mean(tibble::tibble(x = numeric(), y = numeric()), weights = c(x = 0, y = 1))) == 0)
  testthat::expect_true(length(calculate_harmonic_mean(tibble::tibble(x = numeric(), y = numeric()), weights = c(x = 0, y = 1))) == 0)
  testthat::expect_true(length(calculate_geometric_mean(tibble::tibble(x = numeric(), y = numeric()), weights = c(x = 0, y = 1))) == 0)
})
