context("Testing tibble helpers")

li <- list(
  list(a = 1, b = log10, c = "parrot", d = c(1,2,3)) %>% add_class("myobject"),
  list(a = 2, b = sqrt, c = "quest", d = c("a", "b", "c")) %>% add_class("yourobject")
)

tib <- tibble(
  a = c(1, 2),
  b = list(log10, sqrt),
  c = c("parrot", "quest"),
  d = list(c(1,2,3), c("a", "b", "c")),
  .object_class = list(c("myobject", "list"), c("yourobject", "list"))
)

test_that("Testing whether list_as_tibble works", {
  tib2 <- list_as_tibble(li)

  expect_equal(attributes(tib2), attributes(tib))

  expect_equal(tib2$a, tib$a)
  expect_equal(tib2$b, tib$b)
  expect_equal(tib2$c, tib$c)
  expect_equal(tib2$d, tib$d)
  expect_equal(tib2$.object_class, tib$.object_class)
})

test_that("Testing whether tibble_as_list works", {
  li2 <- tibble_as_list(tib)

  expect_equal(li2, li)
})


test_that("Testing whether extract_row_to_list works", {
  for (i in seq_len(nrow(tib))) {
    expect_equal(extract_row_to_list(tib, i), li[[i]])
  }

  expect_null(extract_row_to_list(NULL, 1))
})
