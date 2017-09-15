#' Calculate Earth Mover's Distance between cells in a trajectory
#'
#' @param traj the trajectory
#'
#' @importFrom igraph graph_from_data_frame E distances
#' @importFrom transport transport
#' @import dplyr
#' @importFrom purrr %>% map map_df map_lgl
#' @export
compute_emlike_dist <- function(traj) {
  cell_ids <- traj$cell_ids
  milestone_network <- traj$milestone_network
  milestone_ids <- traj$milestone_ids
  milestone_percentages <- traj$milestone_percentages

  # calculate the shortest path distances between milestones
  phantom_edges <- bind_rows(lapply(milestone_ids, function(sn) {
    sn_filt <- milestone_network %>% filter(from == sn)
    dis_vec <- setNames(sn_filt$length, sn_filt$to)
    phantom_edges <-
      expand.grid(from = sn_filt$to, to = sn_filt$to, stringsAsFactors = FALSE) %>%
      filter(from < to) %>%
      left_join(milestone_network, by = c("from", "to")) %>%
      filter(is.na(length)) %>%
      mutate(length = dis_vec[from] + dis_vec[to])
    phantom_edges
  }))
  gr <- igraph::graph_from_data_frame(bind_rows(milestone_network %>% mutate(length = 2 * length), phantom_edges), directed = FALSE, vertices = milestone_ids)
  milestone_distances <- igraph::distances(gr, weights = igraph::E(gr)$length, mode = "all")

  # transport percentages data
  milestone_percentages$milestone_id = factor(milestone_percentages$milestone_id, levels=milestone_ids) # make sure all milestones are included, even if none of the cells have a value for the milestone
  pct <- reshape2::acast(milestone_percentages, cell_id ~ milestone_id, value.var = "percentage", fill = 0, drop=FALSE)
  pct <- pct[cell_ids, milestone_ids]

  fromto_matrix <- matrix(0, nrow = length(milestone_ids), ncol = length(milestone_ids), dimnames = list(milestone_ids, milestone_ids))
  fromto2 <- milestone_network %>% reshape2::acast(from ~ to, value.var = "length", fun.aggregate = length)
  fromto_matrix[rownames(fromto2), colnames(fromto2)] <- fromto2
  diag(fromto_matrix) <- 1
  fromto_matrix[fromto_matrix > 0] <- 1

  froms <- setNames(lapply(rownames(pct), function(xi) {
    x <- pct[xi,]
    notzero <- x != 0
    wh <-
      if (sum(notzero) == 1) {
        which(notzero)
      } else {
        apply(fromto_matrix, 1, function(y) {
          all(!notzero | y)
        })
      }
    milestone_ids[wh]
  }), rownames(pct))

  closest <- bind_rows(lapply(milestone_ids, function(mid) {
    sample_node <- froms %>% map_lgl(~ mid %in% .)
    if (sum(sample_node) == 0) {
      NULL
    } else {
      milestones <- which(fromto_matrix[mid,] == 1)
      dist_milestones <- milestone_distances[milestones, milestones, drop = FALSE]
      sample_pcts <- pct[sample_node, milestones, drop = FALSE]
      closest_to_nodes <-
        sample_pcts %*% dist_milestones %>%
        reshape2::melt(varnames = c("from", "to"), value.name = "length") %>%
        mutate(from = as.character(from), to = as.character(to))

      closest_to_samples <-
        expand.grid(from = rownames(sample_pcts), to = rownames(sample_pcts), stringsAsFactors = FALSE) %>%
        filter(from < to)
      closest_to_samples$length <- sapply(seq_len(nrow(closest_to_samples)), function(xi) {
        a <- sample_pcts[closest_to_samples$from[[xi]],]
        b <- sample_pcts[closest_to_samples$to[[xi]],]
        diff <- a - b
        which_from <- diff < 0
        num_from <- sum(which_from)
        which_to <- diff > 0
        num_to <- sum(which_to)

        if (num_from == 1) {
          sum(dist_milestones[which_from, which_to] * diff[which_to])
        } else if (num_to == 1) {
          -sum(dist_milestones[which_from, which_to] * diff[which_from])
        } else if (num_from == 0) {
          0
        } else {
          suppressWarnings({
            transport::transport(a, b, costm = dist_milestones) %>%
              mutate(dist = dist_milestones[cbind(from,to)], mult = mass * dist) %>%
              .$mult %>%
              sum
          })
        }
      })

      bind_rows(closest_to_nodes, closest_to_samples)
    }
  }))

  gr2 <- igraph::graph_from_data_frame(closest, directed = FALSE, vertices = c(milestone_ids, cell_ids))
  gr2 %>% igraph::distances(v = cell_ids, to = cell_ids, weights = igraph::E(gr2)$length)
}
