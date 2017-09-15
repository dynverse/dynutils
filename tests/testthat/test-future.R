context("Future helper")

test_that("Future helper works correctly", {
  test_fun <- function(sleeper_time, wait_time) {
    dry_run <- wait_or_kill({1}, wait_time = 5, function(x) x, 1)
    wait_or_kill(
      expr = {
        Sys.sleep(sleeper_time)
        dplyr::data_frame(result = "finished", time = sleeper_time)
      },
      wait_time = wait_time,
      cancel_output_fun = function(t) dplyr::data_frame(result = "killed", time = t),
      check_interval = 1
    )
  }

  out1 <- test_fun(10, 5)
  expect_equal(out1$result, "killed")
  expect_gte(out1$time, 5)

  out2 <- test_fun(5, 10)
  expect_equal(out2$result, "finished")
  expect_gte(out2$time, 5)

  # Also in parallel settings
  sleeper_times <- sample(c(runif(10, .5, 3), runif(10, 7, 10)))
  wait_time <- 4

  parallelMap::parallelStartMulticore(cpus = 2, show.info = TRUE)
  outp <- dplyr::bind_rows(parallelMap::parallelMap(test_fun, sleeper_times, more.args = list(wait_time = wait_time)))
  parallelMap::parallelStop()

  expected_result <- ifelse(sleeper_times < wait_time, "finished", "killed")
  expect_lte( mean(outp$result == expected_result), .9 ) # allow some tests to fail
  expect_true( all(outp$time >= pmin(sleeper_times, wait_time)) )
})
