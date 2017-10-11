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

#' Simplify an igraph network such that consecutive linear edges are removed
#'
#' @param gr an igraph object
#' @importFrom igraph degree V are_adjacent neighbors delete.vertices add.edges
#'
#' @export
#'
#' @examples
#' net <- data.frame(from=1:2, to=2:3, length=1, directed=TRUE, stringsAsFactors = F)
#' gr <- igraph::graph_from_data_frame(net)
#' simplify_igraph_network(gr)
simplify_igraph_network <- function(gr) {
  degr_in <- igraph::degree(gr, mode = "in")
  degr_out <- igraph::degree(gr, mode = "out")
  is_loop <- sapply(igraph::V(gr), function(n) igraph::are_adjacent(gr, n, n))

  is_simple <- degr_in == 1 & degr_out == 1 & !is_loop
  while (any(is_simple)) {
    node <- first(which(is_simple))
    in_nodes <- igraph::neighbors(gr, node, mode = "in") %>% names
    out_nodes <- igraph::neighbors(gr, node, mode = "out") %>% names
    gr <- gr %>% igraph::delete.vertices(node) %>% igraph::add.edges(c(in_nodes, out_nodes))

    degr_in <- igraph::degree(gr, mode = "in")
    degr_out <- igraph::degree(gr, mode = "out")
    is_loop <- sapply(igraph::V(gr), function(n) igraph::are_adjacent(gr, n, n))

    is_simple <- degr_in == 1 & degr_out == 1 & !is_loop
  }

  gr
}
