context("Random time string helper")

test_that("Generating random strings", {
  random_strings <- sapply(seq_len(10), function(x) random_time_string())
  expect_equal(length(unique(random_strings)), length(random_strings))

  expect_true(all(grepl("\\d\\d\\d\\d_\\d\\d_\\d\\d_\\d\\d_\\d\\d_\\d\\d__\\d+", random_strings)))
})
