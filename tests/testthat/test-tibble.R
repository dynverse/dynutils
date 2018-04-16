context("Tibble helper")

test_that("Tibble helpers", {
  l <- list(
    list(a = 1, b = log10, c = c(1,2,3)) %>% add_class("una"),
    list(a = 2, b = sqrt, c = c("684466446", "wkcuoweh", "ufiwhfuow")) %>% add_class("banana")
  )
  tib <- list_as_tibble(l)
  first_row <- extract_row_to_list(tib, 1)
  second_row <- extract_row_to_list(tib, 2)

  expect_equal( nrow(tib), 2 )
  expect_equal( ncol(tib), 4 )

  for (i in seq_len(nrow(tib))) {
    expect_equal( extract_row_to_list(tib, i), l[[i]] )
  }

  expect_null(extract_row_to_list(NULL, 1))
})
