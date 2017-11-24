#' Wrapping a TI task dataset
#'
#' @inheritParams abstract_data_wrapper
#' @param counts the counts
#' @param expression the normalised expression
#' @param cell_info extra information about the cells
#' @param feature_info extra information about the genes
#'
#' @export
wrap_ti_task_data <- function(
  ti_type,
  id,
  counts,
  expression,
  cell_ids,
  milestone_ids,
  milestone_network,
  milestone_percentages = NULL,
  progressions = NULL,
  cell_info = NULL,
  feature_info = NULL,
  ...
) {
  testthat::expect_equal(rownames(counts), cell_ids)
  testthat::expect_equal(rownames(counts), expression)

  if (!is.null(cell_info)) {
    testthat::expect_equal(cell_info$id, cell_ids)
  }
  if (!is.null(gene_info)) {
    testthat::expect_equal(colnames(counts), gene_info$id)
    testthat::expect_equal(colnames(expression), gene_info$id)
  }

  abstract_data_wrapper(
    "ti_task",
    ti_type,
    id,
    cell_ids,
    milestone_ids,
    milestone_network,
    milestone_percentages,
    progressions,
    counts = counts,
    expression = expression,
    cell_info = cell_info,
    feature_info = feature_info,
    ...
  )
}

#' Wrapping TI predictions
#'
#' @inheritParams abstract_data_wrapper
#'
#' @export
wrap_ti_prediction <- function(
  ti_type,
  id,
  cell_ids,
  milestone_ids,
  milestone_network,
  milestone_percentages = NULL,
  progressions = NULL,
  ...
) {
  abstract_data_wrapper(
    "ti_pred",
    ti_type,
    id,
    cell_ids,
    milestone_ids,
    milestone_network,
    milestone_percentages,
    progressions,
    ...
  )
}

#' Wrap a linear TI prediction
#'
#' This function will generate the milestone_network and progressions.
#'
#' @param pseudotimes The pseudotimes of the \code{cell_ids}
#' @inheritParams abstract_data_wrapper
#'
#' @export
wrap_linear_ti_prediction <- function(
  id,
  cell_ids,
  pseudotimes,
  ...
) {
  pseudotimes <- scale_minmax(pseudotimes)
  milestone_ids <- c("milestone_A", "milestone_B")
  milestone_network <- data_frame(
    from = milestone_ids[[1]],
    to = milestone_ids[[2]],
    length = 1,
    directed = FALSE
  )
  progressions <- data_frame(
    cell_id = cell_ids,
    from = milestone_ids[[1]],
    to = milestone_ids[[2]],
    percentage = pseudotimes
  )

  wrap_ti_prediction(
    ti_type = "linear",
    id = id,
    cell_ids = cell_ids,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    progressions = progressions,
    pseudotimes = pseudotimes,
    ...
  )
}

#' An abstract data wrapper
#'
#' @param type the type of data (e.g. ti_task, ti_toy, ti_pred)
#' @param ti_type a descriptor specifying the TI type
#' @param id a unique identifier for the task
#' @param cell_ids the ids of the cells in the trajectory and counts
#' @param milestone_ids the ids of the milestones in the trajectory
#' @param milestone_network the network of the milestones
#' @param milestone_percentages what percentage of milestone is each cell
#' @param progressions what progression does a cell have
#' @param ... extra information to be stored in the wrapper
#'
#' @export
abstract_data_wrapper <- function(
  type,
  ti_type,
  id,
  cell_ids,
  milestone_ids,
  milestone_network,
  milestone_percentages = NULL,
  progressions = NULL,
  ...
) {
  if (!is.data.frame(milestone_network) || ncol(milestone_network) != 4 || any(colnames(milestone_network) != c("from", "to", "length", "directed"))) {
    stop(sQuote("milestone_network"), " should be a data frame with exactly three columns named ", sQuote("from"),
         ", ", sQuote("to"), ", ", sQuote("length"), " and ", sQuote("directed"), ".")
  }
  if (any(!milestone_network$from %in% milestone_ids) || any(!milestone_network$to %in% milestone_ids)) {
    stop("Not all states in ", sQuote("milestone_network"), " are in ", sQuote("milestone_ids"), ".")
  }

  if (is.null(milestone_percentages) == is.null(progressions)) {
    stop("Exactly one of ", sQuote("milestone_percentages"), " or ", sQuote("progressions"), " must be defined, the other must be NULL.")
  }

  if (is.null(progressions)) {
    progressions <- convert_milestone_percentages_to_progressions(
      cell_ids,
      milestone_ids,
      milestone_network,
      milestone_percentages
    )
  } else if (is.null(milestone_percentages)) {
    milestone_percentages <- convert_progressions_to_milestone_percentages(
      cell_ids,
      milestone_ids,
      milestone_network,
      progressions
    )
  }
  eps <- 1e-8

  if (!is.data.frame(milestone_percentages) || ncol(milestone_percentages) != 3 || any(colnames(milestone_percentages) != c("cell_id", "milestone_id", "percentage"))) {
    stop(sQuote("milestone_percentages"), " should be a data frame with exactly four columns named ", sQuote("cell_id"),
         ", ", sQuote("milestone_id"), " and ", sQuote("percentage"), ".")
  }
  if (!is.data.frame(progressions) || ncol(progressions) != 4 || any(colnames(progressions) != c("cell_id", "from", "to", "percentage"))) {
    stop(sQuote("progressions"), " should be a data frame with exactly four columns named ", sQuote("cell_id"),
         ", ", sQuote("from"), ", ", sQuote("to"), " and ", sQuote("percentage"), ".")
  }

  if (any(progressions$percentage < (0 - eps) | progressions$percentage > (1 + eps))) {
    stop(sQuote("progressions"), " percentages should lie between [0,1]")
  }

  if (any(milestone_percentages$percentage < (0 - eps) | milestone_percentages$percentage > (1 + eps))) {
    stop(sQuote("milestone_percentages"), " percentages should lie between [0,1]")
  }

  pr_check <- progressions %>% group_by(cell_id) %>% summarise(sum = sum(percentage))
  if (any(pr_check$sum < (0 - eps) | pr_check$sum > (1 + eps))) {
    stop("The sum of ", sQuote("progressions"), " percentages per cell should lie between [0,1]")
  }

  mp_check <- milestone_percentages %>% group_by(cell_id) %>% summarise(sum = sum(percentage))
  if (any(abs(mp_check$sum - 1) > eps)) {
    stop("The sum of ", sQuote("milestone_percentages"), " percentages per cell should be exactly 1")
  }

  ## create a separate state if some cells have been filtered out
  na_ids <- setdiff(cell_ids, unique(milestone_percentages$cell_id))
  if (length(na_ids) != 0) {
    directed <- any(milestone_network$directed)
    new_mid <- "FILTERED_CELLS"

    milestone_percentages <- milestone_percentages %>% add_row(
      cell_id = na_ids,
      milestone_id = new_mid,
      percentage = 1
    )
    progressions <-  progressions %>% add_row(
      cell_id = na_ids,
      from = new_mid,
      to = new_mid,
      percentage = 1
    )
    milestone_network <- milestone_network %>% add_row(
      from = milestone_ids,
      to = new_mid,
      length = max(milestone_network$length)*5,
      directed = directed
    )
    milestone_ids <- c(milestone_ids, new_mid)
  }

  # create output structure
  out <- list(
    type = type,
    ti_type = ti_type,
    id = id,
    cell_ids = cell_ids,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    milestone_percentages = milestone_percentages,
    progressions = progressions,
    ...
  )
  class(out) <- c("dynutils::ti_data_wrapper", "list")
  out
}

#' Test whether something is a ti_data_wrapper or not
#'
#' @param object The object to be tested.
#'
#' @export
is_ti_data_wrapper <- function(object) {
  "dynutils::ti_data_wrapper" %in% class(object)
}

