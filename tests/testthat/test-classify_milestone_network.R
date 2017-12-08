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
    "shuffled" = data_frame(from = c("A", "A", "B"), to = c("C", "B", "C"), length = 1, directed = TRUE),
    "long" = {
      ord <- sample(LETTERS)
      data_frame(from = ord, to = c(ord[-length(ord)], ord[[1]]), length = 1, directed = FALSE)
    }
  ),
  "undirected_cycle" = list(
    "simple" = data_frame(from = c("A", "B", "C"), to = c("B", "C", "A"), length = 1, directed = FALSE),
    "shuffled" = data_frame(from = c("A", "A", "B"), to = c("C", "B", "C"), length = 1, directed = FALSE),
    "long" = {
      ord <- sample(LETTERS)
      data_frame(from = ord, to = c(ord[-length(ord)], ord[[1]]), length = 1, directed = FALSE)
    }
  )
)

for (network_type in names(all_networks)) {
  networks <- all_networks[[network_type]]

  for (network_name in names(networks)) {
    testthat(pritt("test whether {network_name} is detected as {network_type}"), {
      network <- networks[[network_name]]

      detected_network_type <- classify_milestone_network(network)
      expect_equal(detected_network_type, network_type)
    })
  }
}
