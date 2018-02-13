#' Calculate geodesic distances between cells in a trajectory, taking into account tents
#'
#' @param trajectory the trajectory
#'
#' @importFrom igraph graph_from_data_frame neighborhood E distances
#' @export
compute_tented_geodesic_distances <- function(trajectory) {
  # gather data from trajectory and rename milestones to avoid name conflicts between cells and milestones
  cell_ids <- trajectory$cell_ids
  milestone_network <- trajectory$milestone_network %>% mutate(from = paste0("MILESTONE_", from), to = paste0("MILESTONE_", to))
  milestone_ids <- trajectory$milestone_ids %>% paste0("MILESTONE_", .)
  milestone_percentages <- trajectory$milestone_percentages %>% mutate(milestone_id = paste0("MILESTONE_", milestone_id))

  # construct igraph object of milestone network
  is_directed <- any(milestone_network$directed)
  mil_gr <- igraph::graph_from_data_frame(milestone_network, directed = is_directed, vertices = milestone_ids)

  # calculate cell-cell distances for pairs of cells that are in the same tent
  cell_in_tent_distances <-
    map_df(milestone_ids, function(mid) {
      tent <- names(igraph::neighborhood(mil_gr, nodes = mid, mode = "out")[[1]])

      if (length(tent) <= 1) {
        return(NULL)
      }

      tent_nomid <- setdiff(tent, mid)
      tent_distances <- igraph::distances(mil_gr, v = mid, to = tent, mode = "out")

      relevant_pct <- milestone_percentages %>%
        group_by(cell_id) %>%
        filter(all(milestone_id %in% tent)) %>%
        ungroup()

      if (nrow(relevant_pct) <= 1) {
        return(NULL)
      }

      scaled_dists <-
        relevant_pct %>%
        filter(milestone_id != mid) %>%
        mutate(dist = percentage * tent_distances[mid, milestone_id])

      pct_mat <-
        bind_rows(
          scaled_dists %>% select(from = cell_id, to = milestone_id, length = dist),
          tent_distances %>% as.data.frame() %>% gather(from, length) %>% mutate(to = from)
        ) %>%
        reshape2::acast(from ~ to, value.var = "length", fill = 0)

      dist(pct_mat) %>%
        as.matrix() %>%
        reshape2::melt(varnames = c("from", "to"), value.name = "length") %>%
        mutate(from = as.character(from), to = as.character(to)) %>%
        filter(from > to | from %in% milestone_ids)
    })


  # calculate shortcut edges between milestones. E.g. if A->B and A->C, then B->C will also be added
  shortcut_edges <- bind_rows(lapply(milestone_ids, function(mid) {
    tent <- names(igraph::neighborhood(mil_gr, nodes = mid, mode = "out")[[1]])

    if (length(tent) <= 1) {
      return(NULL)
    }

    tent_nomid <- setdiff(tent, mid)
    tent_distances <- igraph::distances(mil_gr, v = mid, to = tent_nomid, mode = "out") %>%
      as.data.frame %>%
      gather(milestone_id, dist)

    crossing(from = tent_distances$milestone_id, to = tent_distances$milestone_id) %>%
      filter(from < to) %>%
      left_join(tent_distances %>% select(from = milestone_id, from_dist = dist), by = "from") %>%
      left_join(tent_distances %>% select(to = milestone_id, to_dist = dist), by = "to") %>%
      mutate(dist = (from_dist + to_dist) / 2)
  }))

  # combine all networks into one graph
  gr <-
    bind_rows(milestone_network, cell_in_tent_distances, shortcut_edges) %>%
    group_by(from, to) %>%
    summarise(length = min(length)) %>%
    ungroup() %>%
    igraph::graph_from_data_frame(directed = FALSE, vertices = c(milestone_ids, cell_ids))

  # compute cell-to-cell distances across entire graph
  gr %>% igraph::distances(v = cell_ids, to = cell_ids, weights = igraph::E(gr)$length)
}
