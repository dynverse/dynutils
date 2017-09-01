context("Simplify network")

test_that("Testing simplify_network", {
  net <- dplyr::tibble(from=1:2, to=2:3, length=1)

  newnet <- simplify_network(net)
  expect_equal(nrow(newnet), 1)
  expect_equal(newnet$from, 1)
  expect_equal(newnet$to, 3)
  expect_equal(newnet$length, 2)
})
