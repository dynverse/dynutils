context("Future helper")

test_that("Future helper works correctly", {
  test_fun <- function(sleeper_time, wait_time) {
    dry_run <- dyneval:::wait_or_kill({1}, wait_time = 5, function(x) x, 1)
    dyneval:::wait_or_kill(
      expr = {
        Sys.sleep(sleeper_time)
        data_frame(result = "finished", time = sleeper_time)
      },
      wait_time = wait_time,
      cancel_output_fun = function(t) data_frame(result = "killed", time = t),
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
  library(parallelMap)
  sleeper_times <- sample(c(seq(1,5), seq(15,20)), 20, replace = T)
  wait_time <- 10
  parallelStartMulticore(cpus = 2, show.info = TRUE)
  outp <- bind_rows(parallelMap(test_fun, sleeper_times, more.args = list(wait_time = wait_time)))
  parallelStop()
  expected_result <- ifelse(sleeper_times < wait_time, "finished", "killed")
  expect_true( all(outp$result == expected_result) )
  expect_true( all(outp$time >= pmin(sleeper_times, wait_time)) )
})
