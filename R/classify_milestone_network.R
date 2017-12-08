#' Classify a milestone network
#'
#' @param milestone_network A milestone network
#'
#' @export
#'
#' @importFrom igraph graph_from_data_frame
classify_milestone_network <- function(milestone_network) {
  is_directed <- any(milestone_network$directed)

  gr <- igraph::graph_from_data_frame(milestone_network, directed = is_directed) %>%
    simplify_igraph_network()

  props <- determine_milenet_props(gr)

  network_type <- determine_network_type(props)

  lst(network_type, properties = props)
}

determine_network_type <- function(props) {
  with(props, {
    if (is_directed) {
      if (!has_cycles) {
        if (num_branch_nodes == 0) {
          "directed_linear"
        } else if (num_branch_nodes == 1) {
          if (num_convergences == 0) {
            if (max_degree == 3) {
              "bifurcation"
            } else {
              "multifurcation"
            }
          } else {
            "directed_acyclic_graph"
          }
        } else {
          "directed_acyclic_graph"
        }
      } else {
        if (num_branch_nodes == 0) {
          "directed_cycle"
        } else
          "directed_graph"
      }
    } else {
      if (!has_cycles) {
        if (num_branch_nodes == 0) {
          "undirected_linear"
        } else if (num_branch_nodes == 1) {
          if (max_degree == 3) {
            "simple_fork"
          } else {
            "complex_fork"
          }
        } else {
          "unrooted_tree"
        }
      } else {
        if (num_branch_nodes == 0) {
          "undirected_cycle"
        } else {
          "undirected_graph"
        }
      }
    }
  })
}


determine_milenet_props <- function(gr) {
  requireNamespace("igraph")

  is_directed <- igraph::is_directed(gr)

  if (is_directed) {
    degr_in <- igraph::degree(gr, mode = "in")
    degr_out <- igraph::degree(gr, mode = "out")
    degr_tot <- degr_in + degr_out
  } else {
    degr_tot <- igraph::degree(gr)
  }

  is_self_loop <- sapply(igraph::V(gr), function(n) igraph::are_adjacent(gr, n, n))

  max_degree <- max(degr_tot)

  if (is_directed) {
    is_begin <- degr_in == is_self_loop & degr_out != 0
    is_end <- degr_in != 0 & degr_out == 0
    is_branch <- !is_begin & !is_end
    is_outer <- is_begin | is_end
    num_begin_nodes <- sum(is_begin)
    num_end_nodes <- sum(is_end)
    num_branch_nodes <- sum(is_branch)
    num_outer_nodes <- sum(is_outer)
  } else {
    is_outer <- degr_tot == 1
    num_outer_nodes <- sum(is_outer)

    is_branch <- !is_outer
    num_branch_nodes <- sum(is_branch)
  }

  num_divergences <- sum(degr_in != 0 & degr_out > 1)
  num_convergences <- sum(degr_in > 1 & degr_out != 0)
  has_cycles <- !igraph::is.dag(gr)

  out <- lst(
    is_directed,
    max_degree,
    degr_tot,
    is_self_loop,
    num_divergences,
    num_convergences,
    has_cycles,
    is_branch,
    num_branch_nodes,
    is_outer,
    num_outer_nodes
  )

  if (is_directed) {
    c(out, lst(
      degr_in,
      degr_out,
      is_begin,
      is_end,
      num_begin_nodes,
      num_end_nodes,
      num_branch_nodes
    ))
  } else {
    out
  }
}
