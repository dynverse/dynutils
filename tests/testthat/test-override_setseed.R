context("Overriding the set.seed function")

test_that("Overriding the set.seed function", {
  test_message <- "test set seed"
  orig_set_seed <- base::set.seed

  override_setseed(function(i) test_message)

  expect_equal( set.seed(1), test_message )

  override_setseed(orig_set_seed)
})
