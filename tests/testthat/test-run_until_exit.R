context("Testing run_until_exit")

test_that("Testing run_until_exit", {
  out <- run_until_exit("echo hi")
  expect_equal(out$output, "hi")
  expect_equal(out$error, character(0))

  out <- run_until_exit("echo foo 1>&2")
  expect_equal(out$output, character(0))
  expect_equal(out$error, "foo")

  expect_error(run_until_exit("giraffes_are_the_best"))

  out <- run_until_exit("sleep 1; echo foo; echo bar 1>&2")
  expect_equal(out$output, "foo")
  expect_equal(out$error, "bar")

  out <- run_until_exit("sleep 1; echo foo; echo bar 1>&2")
  expect_equal(out$output, "foo")
  expect_equal(out$error, "bar")
})
