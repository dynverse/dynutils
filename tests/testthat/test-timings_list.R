context("Timings List Helper")

test_that("Timings List work in simple use cases", {
  xxx <- list(a = 10, b = 10)

  tl <- add_timing_checkpoint(NULL, "start")
  Sys.sleep(1)
  tl <- tl %>% add_timing_checkpoint("second")
  Sys.sleep(.5)
  tl <- tl %>% add_timing_checkpoint("third")
  Sys.sleep(.25)
  tl <- tl %>% add_timing_checkpoint("stop")

  yyy <- xxx %>% attach_timings_attribute(tl)

  attr_yyy <- get_timings_attribute(yyy)

  testthat::expect_equivalent(xxx, yyy)
  testthat::expect_equivalent(attr_yyy, tl)
  testthat::expect_equivalent(names(tl), c("start", "second", "third", "stop"))

  diffs <- diff(unlist(attr_yyy))
  testthat::expect_true(all(abs(diffs - c(1, .5, .25)) < .1))
})
