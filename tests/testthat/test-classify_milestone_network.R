context("milestone network classification")


all_networks <- list(
  "directed_linear" = list(
    "simple" = data_frame(from = "A", to = "B", length = 1, directed = TRUE),
    "intermediate" = data_frame(from = c("A", "B"), to = c("B", "C"), length = 1, directed = TRUE),
    "shuffled" = data_frame(from = c("A", "B", "C"), to = c("B", "C", "D"), length = 1, directed = TRUE),
    "long" = {
      ord <- sample(LETTERS)
      data_frame(from = ord[-1], to = ord[-length(ord)], length = 1, directed = TRUE)
    }
  ),
  "undirected_linear" = list(
    "simple" = data_frame(from = "A", to = "B", length = 1, directed = FALSE),
    "intermediate" = data_frame(from = c("A", "B"), to = c("B", "C"), length = 1, directed = FALSE),
    "shuffled" = data_frame(from = c("B", "D", "B"), to = c("A", "C", "C"), length = 1, directed = FALSE),
    "long" = {
      ord <- sample(LETTERS)
      data_frame(from = ord[-1], to = ord[-length(ord)], length = 1, directed = FALSE)
    }
  ),
  "directed_cycle" = list(
    "simple" = data_frame(from = c("A", "B", "C"), to = c("B", "C", "A"), length = 1, directed = TRUE),
    "shuffled" = data_frame(from = c("A", "C", "B"), to = c("B", "A", "C"), length = 1, directed = TRUE),
    "long" = {
      ord <- sample(LETTERS)
      data_frame(from = ord, to = c(ord[-1], ord[[1]]), length = 1, directed = TRUE)
    }
  ),
  "undirected_cycle" = list(
    "simple" = data_frame(from = c("A", "B", "C"), to = c("B", "C", "A"), length = 1, directed = FALSE),
    "shuffled" = data_frame(from = c("A", "A", "B"), to = c("C", "B", "C"), length = 1, directed = FALSE),
    "long" = {
      ord <- sample(LETTERS)
      data_frame(from = ord, to = c(ord[-1], ord[[1]]), length = 1, directed = FALSE)
    }
  ),
  "bifurcation" = list(
    "simple" = data_frame(from = c("A", "B", "B"), to = c("B", "C", "D"), length = 1, directed = TRUE),
    "intermediate" = data_frame(from = c("A", "a", "B", "B", "C", "D"), to = c("a", "B", "C", "D", "c", "d"), length = 1, directed = TRUE),
    "shuffled" = data_frame(from = c("B", "A", "B"), to = c("C", "B" ,"D"), length = 1, directed = TRUE),
    "long" = {
      ord <- sample(LETTERS)
      data_frame(
        from = c(ord[1:4], ord[5:9], ord[c(5,11:15)]),
        to = c(ord[2:5], ord[6:10], ord[c(11,12:16)]),
        length = 1,
        directed = TRUE
      )
    }
  ),
  "simple_fork" = list(
    "simple" = data_frame(from = c("A", "B", "B"), to = c("B", "C", "D"), length = 1, directed = FALSE),
    "intermediate" = data_frame(from = c("A", "a", "B", "B", "C", "D"), to = c("a", "B", "C", "D", "c", "d"), length = 1, directed = FALSE),
    "shuffled" = data_frame(from = c("B", "A", "D"), to = c("C", "B" ,"B"), length = 1, directed = FALSE),
    "long" = {
      ord <- sample(LETTERS)
      data_frame(
        from2 = c(ord[1:4], ord[5:9], ord[c(5,11:15)]),
        to2 = c(ord[2:5], ord[6:10], ord[c(11,12:16)]),
        length = 1,
        directed = FALSE
      ) %>%
        mutate(
          mix = sample(c(T, F), n(), replace = TRUE),
          from = ifelse(mix, from2, to2),
          to = ifelse(mix, to2, from2)
        ) %>%
        select(from, to, length, directed)
    }
  ),
  "multifurcation" = list(
    "simple" = data_frame(from = c("A", "B", "B", "B"), to = c("B", "C", "D", "E"), length = 1, directed = TRUE),
    "intermediate" = data_frame(from = c("A", "a", "B", "B", "B", "C", "D"), to = c("a", "B", "C", "D", "E", "c", "d"), length = 1, directed = TRUE),
    "shuffled" = data_frame(from = c("B", "A", "B", "B"), to = c("C", "B" ,"D", "E"), length = 1, directed = TRUE),
    "many" = data_frame(from = c("root", rep("mid", length(LETTERS))), to = c("mid", LETTERS), length = 1, directed = TRUE),
    "long" = {
      ord <- sample(LETTERS)
      data_frame(
        from = c(ord[1:4], ord[5:9], ord[c(5,11:15)], ord[c(5, 17:20)]),
        to = c(ord[2:5], ord[6:10], ord[c(11,12:16)], ord[c(17, 18:21)]),
        length = 1,
        directed = TRUE
      )
    }
  ),
  "complex_fork" = list(
    "simple" = data_frame(from = c("A", "B", "B", "B"), to = c("B", "C", "D", "E"), length = 1, directed = FALSE),
    "intermediate" = data_frame(from = c("A", "a", "B", "B", "C", "D", "B"), to = c("a", "B", "C", "D", "c", "d", "E"), length = 1, directed = FALSE),
    "shuffled" = data_frame(from = c("B", "A", "D", "B"), to = c("C", "B" ,"B", "E"), length = 1, directed = FALSE),
    "many" = data_frame(from = c("root", rep("mid", length(LETTERS))), to = c("mid", LETTERS), length = 1, directed = FALSE),
    "long" = {
      ord <- sample(LETTERS)
      data_frame(
        from2 = c(ord[1:4], ord[5:9], ord[c(5,11:15)], ord[c(5, 17:20)]),
        to2 = c(ord[2:5], ord[6:10], ord[c(11,12:16)], ord[c(17, 18:21)]),
        length = 1,
        directed = FALSE
      ) %>%
        mutate(
          mix = sample(c(T, F), n(), replace = TRUE),
          from = ifelse(mix, from2, to2),
          to = ifelse(mix, to2, from2)
        ) %>%
        select(from, to, length, directed)
    }
  ),
  "rooted_tree" = list(
    "simple" = data_frame(from = c("A", "B", "B", "C", "C"), to = c("B", "C", "D", "E", "F"), length = 2, directed = TRUE)
  )
  # TODO: other network types
)

for (network_type in names(all_networks)) {
  networks <- all_networks[[network_type]]

  for (network_name in names(networks)) {
    test_that(pritt("test whether {network_name} is detected as {network_type}"), {
      network <- networks[[network_name]]

      detected_network_type <- classify_milestone_network(network)$network_type

      if (detected_network_type != network_type) {
        gr <- igraph::graph_from_data_frame(network, directed = any(network$directed))
        plot(gr)
      }
      expect_equal(detected_network_type, network_type)
    })
  }
}
