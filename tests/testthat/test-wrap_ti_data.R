context("Testing TI data wrappers")

test_that("Testing abstract_data_wrapper", {
  type <- "It"
  id <- "a"
  cell_ids <- c("truth", "universally", "acknowledged", "that", "a", "single")
  milestone_ids <-  c("man", "in", "possession", "of", "good", "fortune", "must")
  milestone_network <- tribble(
    ~from, ~to, ~length, ~directed,
    "man", "in", 1, TRUE,
    "in", "possession", 2, TRUE,
    "in", "of", 3, TRUE,
    "possession", "good", 4, TRUE,
    "of", "fortune", 5, TRUE,
    "good", "must", 6, TRUE,
    "fortune", "must", 7, TRUE
  )
  milestone_percentages <- tribble(
    ~cell_id, ~milestone_id, ~percentage,
    "truth", "man", .8,
    "truth", "in", .2,
    "universally", "in", .3,
    "universally", "possession", .2,
    "universally", "of", .5,
    "acknowledged", "possession", 0,
    "acknowledged", "good", 1,
    "that", "good", .5,
    "that", "must", .5,
    "a", "good", .9,
    "a", "must", .1,
    "single", "fortune", .6,
    "single", "must", .4
  )
  progressions <- tribble(
    ~cell_id, ~from, ~to, ~percentage,
    "truth", "man", "in", .2,
    "universally", "in", "possession", .2,
    "universally", "in", "of", .5,
    "acknowledged", "possession", "good", 1,
    "that", "good", "must", .5,
    "a", "good", "must", .1,
    "single", "fortune", "must", .4
  )
  extras1 <- list("be in want of a wife.")
  extras2 <- "However little known the feelings or views of such a man may be on his first entering a neighbourhood."
  wr <- abstract_data_wrapper(
    type = type,
    id = id,
    cell_ids = cell_ids,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    milestone_percentages = milestone_percentages,
    extras1 = extras1,
    extras2 = extras2
  )

  # testing is_ti_data_wrapper
  expect_true(is_ti_data_wrapper(wr))
  expect_false(is_ti_data_wrapper(list(chvehoie="jihofrewghifu")))

  expect_equivalent(wr$type, type)
  expect_equivalent(wr$id, id)
  expect_equivalent(wr$cell_ids, cell_ids)
  expect_equivalent(wr$milestone_ids, milestone_ids)
  expect_equivalent(wr$milestone_network, milestone_network)
  expect_equivalent(wr$milestone_percentages, milestone_percentages)
  joined <- wr$progressions %>% left_join(progressions, by = c("cell_id", "from", "to")) %>% mutate(diff = abs(percentage.x - percentage.y))
  expect_equivalent(joined$percentage.x, joined$percentage.y)
  expect_equivalent(wr$extras1, extras1)
  expect_equivalent(wr$extras2, extras2)

  # testing progressions to percentages
  wr <- abstract_data_wrapper(
    type = type,
    id = id,
    cell_ids = cell_ids,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    progressions = progressions,
    extras1 = list("be in want of a wife."),
    extras2 = "However little known the feelings or views of such a man may be on his first entering a neighbourhood."
  )
  expect_true(is_ti_data_wrapper(wr))

  expect_equivalent(wr$type, type)
  expect_equivalent(wr$id, id)
  expect_equivalent(wr$cell_ids, cell_ids)
  expect_equivalent(wr$milestone_ids, milestone_ids)
  expect_equivalent(wr$milestone_network, milestone_network)
  joined <- wr$milestone_percentages %>% left_join(milestone_percentages, by = c("cell_id", "milestone_id")) %>% mutate(diff = abs(percentage.x - percentage.y))
  expect_equivalent(joined$percentage.x, joined$percentage.y)
  expect_true( all(abs(joined$diff) < 1e-10) )
  expect_equivalent(wr$progressions, progressions)
  expect_equivalent(wr$extras1, extras1)
  expect_equivalent(wr$extras2, extras2)

  # testing what happens when a cell was filtered
  wr <- abstract_data_wrapper(
    type = type,
    id = id,
    cell_ids = c(cell_ids, "filtered_cell"),
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    milestone_percentages = milestone_percentages,
    extras1 = extras1,
    extras2 = extras2
  )
  expect_equivalent(wr$milestone_percentages %>% filter(cell_id == "filtered_cell") %>% .$milestone_id, "FILTERED_CELLS")
  expect_true(is_ti_data_wrapper(wr))

  # testing cell not on edge
  expect_error(abstract_data_wrapper(
    type = type,
    id = id,
    cell_ids = c(cell_ids, "faulty_cell"),
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    milestone_percentages = bind_rows(milestone_percentages, data_frame(cell_id = "faulty_cell", milestone_id = c("man", "must"), percentage = c(.4, .6))),
    extras1 = extras1,
    extras2 = extras2
  ))

  # testing cell in tent but without unknown from
  expect_error(abstract_data_wrapper(
    type = type,
    id = id,
    cell_ids = c(cell_ids, "faulty_cell"),
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    milestone_percentages = bind_rows(milestone_percentages, data_frame(cell_id = "faulty_cell", milestone_id = c("posession", "of"), percentage = c(.4, .6))),
    extras1 = extras1,
    extras2 = extras2
  ))
})
