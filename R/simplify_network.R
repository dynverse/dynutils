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

#' Simplify a graph of samples
#'
#' @param edges The edges between samples. Should contain columns "from", "to", "length", and "directed"
#' @param is_trajectory A \strong{named} vector containing booleans fo whether or not a sample is to be kept.
#' @param is_directed Whether or not the graph is directed.
#' @export
simplify_sample_graph <- function(edges, is_trajectory, is_directed) {
  requireNamespace("igraph")

  ids <- names(is_trajectory)
  verts <- data_frame(cell_id = ids, from = ids, to = ids, percentage = 1, is_trajectory = is_trajectory)
  gr <- igraph::graph_from_data_frame(edges, directed = is_directed, vertices = verts)

  # remove vertices that are not part of the trajectory
  vs <- igraph::V(gr)
  vs_non_trajs <- vs[!vs$is_trajectory]
  vs_trajs <- vs[vs$is_trajectory]

  # find closest traj-node to non-traj-nodes
  dists <- igraph::distances(gr, vs_non_trajs, vs_trajs)
  corresponding_vs <- names(vs_trajs[apply(dists, 1, which.min)])

  # set from and to for non-traj-nodes to their closest traj-node
  igraph::V(gr)[!vs$is_trajectory]$from <- corresponding_vs
  igraph::V(gr)[!vs$is_trajectory]$to <- corresponding_vs

  # remove edges connecting non-traj-nodes, resulting in the backbone
  ends <- igraph::ends(gr, igraph::E(gr))
  bgr <- igraph::delete.edges(gr, which(ends[,1] %in% names(vs_non_trajs) | ends[,2] %in% names(vs_non_trajs)))

  # simplifying trajectory
  bvs <- igraph::V(bgr)
  bvs <- names(bvs[bvs$is_trajectory])

  # for each node in backbone
  for (id in bvs) {
    # does the node have degree 2 (i.e. should it be removed)
    if (igraph::degree(bgr, id) == 2 && (!is_directed || (igraph::degree(bgr, id, "in") == 1 && igraph::degree(bgr, id, "out") == 1))) {
      # detect the new from and new to
      if (is_directed) {
        newfrom <- igraph::neighbors(bgr, id, mode = "in") %>% names
        newto <- igraph::neighbors(bgr, id, mode = "out") %>% names
      } else {
        neighs <- igraph::neighbors(bgr, id)
        newfrom <- neighs[1] %>% names
        newto <- neighs[2] %>% names
      }
      # if newfrom and newto are already connected, do not remove edges for current node
      if (!igraph::are_adjacent(bgr, newfrom, newto)) {

        # take the relevant part of the backbone. should be exactly two edges
        relevant_bgr <- igraph::as_data_frame(igraph::induced_subgraph(bgr, c(id, newfrom, newto)))

        # find the length of the first and the second edge
        newfrom_length <- relevant_bgr %>% filter(from == newfrom | to == newfrom) %>% .$length
        newto_length <- relevant_bgr %>% filter(from == newto | to == newto) %>% .$length

        # calculate the length of the new edge
        comb_length <- newfrom_length + newto_length

        # add new edge
        new_edge <- igraph::edge(newfrom, newto)
        new_edge$length <- comb_length
        new_edge$directed <- is_directed
        bgr <- (bgr + new_edge)

        # remove old edge
        ends <- igraph::ends(bgr, igraph::E(bgr))
        bgr <- bgr %>% igraph::delete.edges(which(ends[,1] == id | ends[,2] == id))

        # adjust vertex attributes
        vfrom <- igraph::V(bgr)$from
        vto <- igraph::V(bgr)$to
        vpct <- igraph::V(bgr)$percentage
        vs_to_update <- which(vfrom == id | vto == id)

        for (vi in vs_to_update) {
          prevfrom <- vfrom[[vi]]
          prevto <- vto[[vi]]
          vpct_vi <- vpct[[vi]]

          # calculate new percentage depending on
          # what its previous from and to is
          new_pct <-
            if (prevfrom == id && prevto == id) {
              newfrom_length
            } else if (newfrom == prevfrom) {
              vpct_vi * newfrom_length
            } else if (newfrom == prevto) {
              (1 - vpct_vi) * newfrom_length
            } else if (newto == prevfrom) {
              newfrom_length + newto_length * vpct_vi
            } else if (newto == prevto) {
              newfrom_length + newto_length * (1 - vpct_vi)
            } else {
              stop("This should never occur")
            }
          igraph::V(bgr)$from[[vi]] <- newfrom
          igraph::V(bgr)$to[[vi]] <- newto
          igraph::V(bgr)$percentage[[vi]] <- new_pct / comb_length
        }
      }
    }
  }

  # check whether nodes are still assigned to itself.
  # if so, assign it to one of the edges in E(bgr)
  for (v in igraph::V(bgr)) {
    vfrom <- igraph::V(bgr)$from[[v]]
    vto <- igraph::V(bgr)$to[[v]]
    if (vfrom == vto) {
      ends <- igraph::ends(bgr, igraph::E(bgr))
      ei <- which(ends[,2] == vfrom)

      if (length(ei) != 0) {
        ei <- ei[[1]]
        igraph::V(bgr)$from[[v]] <- ends[ei,1]
        igraph::V(bgr)$to[[v]] <- ends[ei,2]
        igraph::V(bgr)$percentage[[v]] <- 1
      } else {
        ei <- which(ends[,1] == vfrom)[[1]]
        igraph::V(bgr)$from[[v]] <- ends[ei,1]
        igraph::V(bgr)$to[[v]] <- ends[ei,2]
        igraph::V(bgr)$percentage[[v]] <- 0
      }

    }
  }

  milestone_network <- igraph::as_data_frame(bgr, "edges") %>%
    mutate(from = paste0("milestone_", from), to = paste0("milestone_", to))

  milestone_ids <- sort(unique(c(milestone_network$from, milestone_network$to)))

  progressions <- igraph::as_data_frame(bgr, "vertices") %>%
    select(-is_trajectory) %>%
    mutate(from = paste0("milestone_", from), to = paste0("milestone_", to))

  lst(milestone_ids, milestone_network, progressions)
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
