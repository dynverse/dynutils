#' Simplify a milestone network, so that consecutive linear edges are merged
#'
#' @param net A network in data.frame format containing at least the columns from, to and length
#' @export
#'
#' @examples
#' net <- data.frame(from=1:2, to=2:3, length=1, directed=TRUE, stringsAsFactors = F)
#' simplify_milestone_network(net)
#'
#' net <- data.frame(from=c(1,2,3, 4), to=c(2,3,4, 1), directed=TRUE, length=1)
#' simplify_milestone_network(net)
simplify_milestone_network = function(net) {
  if (any(!net$directed)) {
    stop("Undirected networks are not supported by this function")
  }

  for (node in unique(c(net$from, net$to))) {
    froms <- net %>% filter(from == node)
    tos <- net %>% filter(to == node)

    if (nrow(froms) == 1 && nrow(tos) == 1) {
      connected <- net %>% filter((from == tos$from & to == froms$to) | (to == tos$from & from == froms$to))

      # check if they are connected in a cycle
      if (nrow(connected) == 0) {
        newfrom <- tos$from
        newto <- froms$to
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

  # check for linear
  if (nrow(net) == 1 && net$from != net$to) {
    net <- tibble(from=c(net$from, ">1"), to=c(">1", net$to), directed=TRUE, length = 0.5)
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
#'
#' net <- data.frame(from=c(1,2,3,1), to=c(2,3,1,4), length=1, directed=TRUE, stringsAsFactors = F)
#' gr <- igraph::graph_from_data_frame(net)
#' simplify_igraph_network(gr)
simplify_igraph_network <- function(gr) {
  is_loop <- igraph::V(gr) %>%
    map_lgl(~ igraph::are_adjacent(gr, ., .))

  # add weight attribute if not already present
  if (!"weight" %in% names(igraph::edge.attributes(gr))) {
    igraph::E(gr)$weight <- 1
  }

  if (igraph::is.directed(gr)) {
    degr_in <- igraph::degree(gr, mode = "in")
    degr_out <- igraph::degree(gr, mode = "out")
    keep_v <- degr_in != 1 | degr_out != 1 | is_loop

    if (sum(keep_v) == 0) {
      # if keep is character(0), gr is a simple cycle
      igraph::graph_from_data_frame(
        data_frame(
          from = names(igraph::V(gr))[[1]],
          to = from,
          weight = sum(igraph::E(gr)$weight)
        ),
        directed = igraph::is.directed(gr)
      )
    } else {
      num_vs <- igraph::V(gr) %>% length

      # igraph::neighbors(
      neighs_in <- seq_len(num_vs) %>% map(~igraph::neighbors(gr, ., mode = "in") %>% as.integer)
      neighs_out <- seq_len(num_vs) %>% map(~igraph::neighbors(gr, ., mode = "out") %>% as.integer)
      to_process <- !keep_v

      edges_to_add <- list()

      for (v_rem in seq_len(num_vs)) {
        if (to_process[[v_rem]]) {
          to_process[[v_rem]] <- FALSE

          # search for in end
          i <- neighs_in[[v_rem]]
          while (to_process[[i]]) {
            to_process[[i]] <- FALSE
            i <- neighs_in[[i]]
          }

          # search for out end
          j <- neighs_out[[v_rem]]
          while (to_process[[j]]) {
            to_process[[j]] <- FALSE
            j <- neighs_out[[j]]
          }

          edges_to_add[[length(edges_to_add)+1]] <- list(from = i, to = j)
        }
      }

      weights_to_add <- sapply(edges_to_add, function(e) igraph::distances(gr, e[[1]], e[[2]])[1,1])
      weights_to_add[weights_to_add == 0] <- sum(igraph::E(gr)$weight)

      gr2 <- gr
      if (length(edges_to_add) > 0) {
        gr2 <- gr2 %>% igraph::add.edges(unlist(edges_to_add), attr = list(weight = weights_to_add, directed=TRUE))
      }
      gr2 %>% igraph::delete.vertices(which(!keep_v))
    }
  } else {
    degr <- igraph::degree(gr)
    keep_v <- degr != 2 | is_loop

    if (sum(keep_v) == 0) {
      # if keep is character(0), gr is a simple cycle
      igraph::graph_from_data_frame(
        data_frame(
          from = names(igraph::V(gr))[[1]],
          to = from,
          weight = sum(igraph::E(gr)$weight)
        ),
        directed = igraph::is.directed(gr)
      )
    } else {
      num_vs <- igraph::V(gr) %>% length

      # igraph::neighbors(
      neighs <- seq_len(num_vs) %>% map(~igraph::neighbors(gr, .) %>% as.integer)
      to_process <- !keep_v

      edges_to_add <- list()

      for (v_rem in seq_len(num_vs)) {
        if (to_process[[v_rem]]) {
          to_process[[v_rem]] <- FALSE

          # search for in end
          i <- neighs[[v_rem]][[1]]
          i_prev <- v_rem
          while (to_process[[i]]) {
            to_process[[i]] <- FALSE
            i_new <- setdiff(neighs[[i]], i_prev)
            i_prev <- i
            i <- i_new
          }

          # search for out end
          j <- neighs[[v_rem]][[2]]
          j_prev <- v_rem
          while (to_process[[j]]) {
            to_process[[j]] <- FALSE
            j_new <- setdiff(neighs[[j]], j_prev)
            j_prev <- j
            j <- j_new
          }

          edges_to_add[[length(edges_to_add)+1]] <- list(from = i, to = j)
        }
      }

      weights_to_add <- sapply(edges_to_add, function(e) igraph::distances(gr, e[[1]], e[[2]])[1,1])
      weights_to_add[weights_to_add == 0] <- sum(igraph::E(gr)$weight)

      gr2 <- gr
      if (length(edges_to_add) > 0) {
        gr2 <- gr2 %>% igraph::add.edges(unlist(edges_to_add), attr = list(weight = weights_to_add, directed=TRUE))
      }
      gr2 %>% igraph::delete.vertices(which(!keep_v))
    }
  }


}
