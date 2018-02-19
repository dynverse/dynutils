#' Add a linear trajectory to a data wrapper
#'
#' This function will generate the milestone_network and progressions.
#'
#' @param data_wrapper A data wrapper to extend upon.
#' @param edges The edges between cells. Format: Data frame(from = character, to = character, length = numeric)
#' @param to_keep A named vector containing booleans containing
#'   whether or not a sample is part of the trajectory that is to be kept.
#' @param is_directed Whether or not the graph is directed.
#' @param ... extra information to be stored in the wrapper.
#'
#' @export
#'
#' @importFrom testthat expect_is expect_true expect_equal expect_length
simplify_sample_graph <- function(
  data_wrapper,
  edges,
  to_keep,
  is_directed,
  ...
) {
  requireNamespace("igraph")

  # check data wrapper
  testthat::expect_is(data_wrapper, "dynutils::data_wrapper")
  cell_ids <- data_wrapper$cell_ids

  # check edges
  testthat::expect_is(edges, "data.frame")
  testthat::expect_equal(colnames(edges), c("from", "to", "length"))
  testthat::expect_equal(sapply(edges, class), c(from = "character", to = "character", length = "numeric"))
  testthat::expect_true(all(edges$from %in% cell_ids))
  testthat::expect_true(all(edges$to %in% cell_ids))

  # check to_keep
  testthat::expect_is(to_keep, "logical")
  testthat::expect_true(all(names(to_keep) %in% cell_ids))
  testthat::expect_equal(sort(unique(c(edges$from, edges$to))), sort(names(to_keep)))

  # check is_directed
  testthat::expect_is(is_directed, "logical")
  testthat::expect_length(is_directed, 1)

  # make igraph object
  ids <- names(to_keep)
  gr <- igraph::graph_from_data_frame(edges %>% rename(weight = length), directed = is_directed, vertices = ids)

  # STEP 1: for each cell, find closest milestone
  v_keeps <- names(to_keep)[to_keep]
  dists <- igraph::distances(gr, to = v_keeps)
  closest_trajpoint <- v_keeps[apply(dists, 1, which.min)]

  # STEP 2: simplify backbone
  gr <- gr %>%
    igraph::induced.subgraph(v_keeps)

  # remove nodes with degree with degree 2, if undirected,
  # or in degree 1 and out degree 1, if directed
  sgr <- simplify_igraph_network(gr)
  milestone_ids <- igraph::V(sgr)$name

  # STEP 3: Calculate progressions of cell_ids

  # determine which nodes were on each path
  milestone_network_proto <-
    sgr %>%
    igraph::as_data_frame() %>%
    as_tibble() %>%
    rowwise() %>%
    mutate(
      path = igraph::shortest_paths(gr, from, to, mode = "out")$vpath %>% map(names)
    ) %>%
    ungroup()

  # for each node, find an edge which contains the node and
  # calculate its progression along that edge
  progressions <-
    milestone_network_proto %>%
    rowwise() %>%
    do(with(., data_frame(from, to, weight, node = path))) %>%
    ungroup %>%
    group_by(node) %>%
    slice(1) %>%
    mutate(
      percentage = igraph::distances(gr, from, node) / weight
    ) %>%
    ungroup() %>%
    right_join(
      data_frame(cell_id = ids, node = closest_trajpoint),
      by = "node"
    ) %>%
    select(cell_id, from, to, percentage)

  # create output
  milestone_network <- milestone_network_proto %>%
    select(from, to, length = weight) %>%
    mutate(directed = is_directed)

  # rename milestones so the milestones don't have the
  # same names as the nodes
  renamefun <- function(x) paste0("milestone_", x)

  milestone_network <- milestone_network %>%
    mutate_at(c("from", "to"), renamefun)
  milestone_ids <- milestone_ids %>%
    renamefun
  progressions <- progressions %>%
    mutate_at(c("from", "to"), renamefun)

  # return output
  add_trajectory_to_wrapper(
    data_wrapper = data_wrapper,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    divergence_regions = NULL,
    progressions = progressions
  )
}
