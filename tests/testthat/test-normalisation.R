context("Normalisation")

test_that("Testing normalise function", {
  counts <- matrix(round(2^rnorm(30*50, mean = 6, sd = 2)), ncol=50)
  counts[sample(c(T, F), length(counts), prob = c("T"=.1, "F"=.9), replace = TRUE)] <- 0
  rownames(counts) <- paste0("Cell", seq_len(nrow(counts)))
  colnames(counts) <- paste0("Gene", seq_len(ncol(counts)))

  # todo: add mitochondrial and ercc spikeins

  normd <- normalise_filter_counts(counts)
})
