context("Simplify network")

test_that("Testing simplify_milestone_network", {
  net <- tibble::tribble(
    ~from, ~to, ~length, ~directed,
    1, 2, 1, T,
    2, 3, 1, T,
    3, 4, 1, T,
    3, 5, 1, T,
    4, 5, 1, T
  )
  newnet <- simplify_milestone_network(net)
  expect_equal(nrow(newnet), 4)
  expect_equal(ncol(newnet), 4)
  expect_true( newnet %>% filter(from == 3, to == 4, length == 1, directed) %>% nrow == 1 )
  expect_true( newnet %>% filter(from == 3, to == 5, length == 1, directed) %>% nrow == 1 )
  expect_true( newnet %>% filter(from == 4, to == 5, length == 1, directed) %>% nrow == 1 )
  expect_true( newnet %>% filter(from == 1, to == 3, length == 2, directed) %>% nrow == 1 )

  net <- tibble::tribble(
    ~from, ~to, ~length, ~directed,
    1, 2, 1, T,
    2, 3, 1, T,
    3, 4, 1, T,
    3, 5, 1, T,
    5, 4, 1, T
  )
  newnet <- simplify_milestone_network(net)
  expect_equal(nrow(newnet), 4)
  expect_equal(ncol(newnet), 4)
  expect_true( newnet %>% filter(from == 3, to == 4, length == 1, directed) %>% nrow == 1 )
  expect_true( newnet %>% filter(from == 3, to == 5, length == 1, directed) %>% nrow == 1 )
  expect_true( newnet %>% filter(from == 5, to == 4, length == 1, directed) %>% nrow == 1 )
  expect_true( newnet %>% filter(from == 1, to == 3, length == 2, directed) %>% nrow == 1 )
})

test_that("Testing simplify_sample_graph", {
  edges <- tibble::tribble(
    ~from, ~to, ~length, ~directed,
    "A", "B", .5, F,
    "B", "C", .6, F,
    "C", "D", .7, F,
    "D", "E", .8, F,
    "D", "F", .9, F,
    "a", "A", .1, F,
    "b", "B", .1, F,
    "bb", "B", .08, F,
    "c", "C", .05, F,
    "cc", "c", .1, F,
    "d", "D", .01, F
  )
  is_trajectory <- c(A = T, B = T, C = T, D = T, E = T, "F" = T, a = F, b = F, bb = F, c = F, cc = F, d = F)
  is_directed <- F

  out <- simplify_sample_graph(edges, is_trajectory, is_directed)

  expect_equal(nrow(out$milestone_network), 3)
  expect_equal(nrow(out$progressions), length(is_trajectory))

  test_strs <- out$milestone_network %>% {paste(.$from, .$to, .$length, .$directed, sep = "|")} %>% sort
  expected_strs <- c(
    "A|D|1.8|FALSE",
    "D|E|0.8|FALSE",
    "D|F|0.9|FALSE"
  ) %>% sort
  expect_equal(test_strs, expected_strs)

  test_strs <- out$progressions %>% {paste(.$name, .$from, .$to, round(.$percentage, 2), sep = "|")} %>% sort
  expected_strs <- c(
    "A|A|D|0",
    "B|A|D|0.28",
    "C|A|D|0.61",
    "D|A|D|1",
    "E|D|E|1",
    "F|D|F|1",
    "a|A|D|0",
    "b|A|D|0.28",
    "bb|A|D|0.28",
    "c|A|D|0.61",
    "cc|A|D|0.61",
    "d|A|D|1"
  ) %>% sort
  expect_equal(test_strs, expected_strs)

})

test_that("Testing simplify_igraph_network", {
  net <- tibble::tribble(
    ~from, ~to,
    1, 2,
    2, 3,
    3, 4,
    3, 5,
    4, 5
  )
  gr <- igraph::graph_from_data_frame(net)

  newgr <- simplify_igraph_network(gr)
  newnet <- igraph::as_data_frame(newgr)
  expect_equal(nrow(newnet), 3)
  expect_equal(ncol(newnet), 2)
  expect_true( newnet %>% filter(from == "1", to == "3") %>% nrow == 1 )
  expect_true( newnet %>% filter(from == "3", to == "5") %>% nrow == 2 )
})
