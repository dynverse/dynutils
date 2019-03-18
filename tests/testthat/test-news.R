context("Testing news helpers")


test_that("update_news works", {
  news_normal <- update_news("dynutils", write = FALSE)

  expect_is(news_normal, "character")
})


test_that("recent_news works", {
  recent_news <- recent_news("dynutils", 10)

  expect_is(recent_news, "character")
})

