#' Simplify a milestone network, so that consecutive linear edges are merged
#'
#' @param net A network in data.frame format containing at least the columns from, to and length
#' @export
#'
#' @examples
#' net <- data.frame(from=1:2, to=2:3, length=1, directed=TRUE, stringsAsFactors = F)
#' simplify_milestone_network(net)
simplify_milestone_network = function(net) {
  for (node in unique(c(net$from, net$to))) {
    froms <- net %>% filter(from == node)
    tos <- net %>% filter(to == node)

    if (nrow(froms) == 1 && nrow(tos) == 1) {
      newfrom <- tos$from
      newto <- froms$to

      # special check for A->B A->C C->B ("split and converge pattern")
      if(!(paste0(newfrom, "#", newto) %in% paste0(net$from, "#", net$to))) {
        net <- net %>%
          filter(from != node, to != node) %>%
          add_row(
            from = newfrom,
            to = newto,
            length = froms$length + tos$length,
            directed = froms$directed
          )
      }
    }
  }

  net
}

# edges <- tibble::tribble(
#   ~from, ~to, ~length, ~directed,
#   "A", "B", 1, F,
#   "C", "B", 2, F,
#   "C", "D", 3, F,
#   "E", "D", 4, F,
#   "E", "F", 5, F,
#   "D", "G", 6, F,
#   "a", "A", .1, F,
#   "b", "B", .2, F,
#   "bb", "b", .3, F,
#   "c", "C", .5, F,
#   "cc", "C", .1, F
# )
# sams <- unique(c(edges$from, edges$to))
# to_keep <- setNames(toupper(sams) == sams, sams)
# is_directed <- F

#' Simplify a graph of samples
#'
#' @param edges The edges between samples. Should contain columns "from", "to", "length", and "directed"
#' @param to_keep A \strong{named} vector containing booleans containing
#'   whether or not a sample is part of the trajectory that is to be kept.
#' @param is_directed Whether or not the graph is directed.
#' @export
simplify_sample_graph <- function(edges, to_keep, is_directed) {
  requireNamespace("igraph")

  ids <- intersect(names(to_keep), unique(c(edges$from, edges$to)))
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
  milestone_ids <- renamefun(milestone_ids)
  progressions <- progressions %>%
    mutate_at(c("from", "to"), renamefun)

  lst(
    milestone_ids,
    milestone_network,
    progressions
  )
}

#' Simplify an igraph network such that consecutive linear edges are removed
#'
#' @param gr an igraph object
#'
#' @importFrom igraph V are_adjacent is.directed degree graph_from_data_frame distances
#'
#' @export
#'
#' @examples
#' net <- data.frame(from=1:2, to=2:3, length=1, directed=TRUE, stringsAsFactors = F)
#' gr <- igraph::graph_from_data_frame(net)
#' simplify_igraph_network(gr)
simplify_igraph_network <- function(gr) {
  is_loop <- igraph::V(gr) %>%
    map_lgl(~ igraph::are_adjacent(gr, ., .))

  # add weight attribute if not already present
  if (!"weight" %in% names(igraph::edge.attributes(gr))) {
    igraph::E(gr)$weight <- 1
  }

  # figure out which nodes to keep
  keep <-
    if (igraph::is.directed(gr)) {
      degr_in <- igraph::degree(gr, mode = "in")
      degr_out <- igraph::degree(gr, mode = "out")
      which(degr_in != 1 | degr_out != 1 | is_loop) %>% names
    } else {
      degr <- igraph::degree(gr)
      which(degr != 2 | is_loop) %>% names
    }

  # determine the paths to keep
  network <-
    crossing(from = keep, to = keep) %>%
    as_tibble() %>%
    rowwise() %>%
    do(with(., tibble(
        from,
        to,
        path = igraph::all_simple_paths(gr, from, to) %>% map(names)
      ))) %>%
    mutate(num_keepers = sum(path %in% keep)) %>%
    filter(num_keepers == 2 | (from == to & is_loop[from])) %>%
    mutate(
      weight = sum(igraph::E(gr, path = path)$weight)
    ) %>%
    select(from, to, weight)

  # remove double edges if network was undirected
  if (!igraph::is.directed(gr)) {
    network <- network %>% filter(from <= to)
  }

  igraph::graph_from_data_frame(network, directed = igraph::is.directed(gr))
}
