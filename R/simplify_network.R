#' Simplify a network, so that consecutive linear edges are merged
#' @param net A network in data.frame format containing at least the columns from, to and length
#' @export
#' @examples
#' net <- dplyr::tibble(from=1:2, to=2:3, length=1)
#' simplify_network(net)
simplify_network = function(net) {
  for (node in unique(c(net$from, net$to))) {
    froms = net %>% filter(from == node)
    tos = net %>% filter(to == node)

    if ((nrow(froms) == 1) && (nrow(tos) == 1)) {
      newfrom = tos$from
      newto = froms$to

      # special check for A->B A->C C->B ("split and converge pattern")
      if(!(paste0(newfrom, "#", newto) %in% paste0(net$from, "#", net$to))) {
        last = node
        net = net %>% filter(from != node) %>% filter(to != node) %>% bind_rows(tibble(from=newfrom, to=newto, length=froms$length+tos$length, directed=froms$directed))
      }
    }
  }

  # remove extra filtered_cell edges
  net %>% filter(!(to == "FILTERED_CELLS"))
}
