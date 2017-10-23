context("Timeout helper")

test_that("Timeouts work in simple use cases", {
  # expect error on timeout
  expect_error(eval_with_timeout({Sys.sleep(10); 1}, timeout = 3))

  # expect correct result when no timeout
  expect_equal(eval_with_timeout({Sys.sleep(1); 5.5}, timeout = 5), 5.5)

  # support practically infinite timeouts
  expect_equal(eval_with_timeout({Sys.sleep(1); 5.5}, timeout = 1e8), 5.5)
})

test_that("Timeouts work in parallel usecases", {
  sleeper_times <- sample(c(runif(10, .5, 3), runif(10, 7, 10)))
  wait_time <- 5

  parallelMap::parallelStartMulticore(cpus = 2, show.info = TRUE)
  outp <- parallelMap::parallelMap(sleeper_times, fun = function(t) {
    eval_with_timeout(
      {
        Sys.sleep(t)
        t
      },
      timeout = wait_time,
      on_timeout = "silent"
    )
  })
  parallelMap::parallelStop()

  outp <- unlist(outp, recursive = FALSE)

  expected_result <- ifelse(sleeper_times <= wait_time, sleeper_times, NA)
  are_same <- abs(outp - expected_result) < 1e-5 | (is.na(outp) & is.na(expected_result))
  expect_true(all(are_same))
})


test_that("Packages loaded in forked environments do not override the global", {
  sess_info_before <- utils::sessionInfo()

  z <- eval_with_timeout(
    {
      library(dplyr)
      library(magrittr)
      library(methods)
      library(purrr)
      library(testthat)
      library(tibble)
      library(tidyr)
      library(reshape2)
      library(igraph)
      library(transport)
      10
    },
    timeout = 20,
    on_timeout = "silent"
  )
  sess_info_after <- utils::sessionInfo()

  expect_equal(z, 10)

  expect_equal(sess_info_after$otherPkgs, sess_info_before$otherPkgs)
})

test_that("Timeouts work in system calls", {
  # expect error on timeout
  expect_error(eval_with_timeout({system("sleep 10"); 1}, timeout = 3))

  # expect correct result when no timeout
  expect_equal(eval_with_timeout({system("sleep 1"); 5.5}, timeout = 5), 5.5)
})
