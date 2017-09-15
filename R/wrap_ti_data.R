#' Wrapping a TI task dataset
#'
#' @inheritParams abstract_data_wrapper
#' @param counts the counts
#' @param sample_info extra information about the cells
#' @param feature_info extra information about the genes
#'
#' @export
wrap_ti_task_data <- function(
  ti_type,
  id,
  counts,
  cell_ids,
  milestone_ids,
  milestone_network,
  milestone_percentages = NULL,
  progressions = NULL,
  sample_info = NULL,
  feature_info = NULL,
  ...
) {
  abstract_data_wrapper(
    "ti",
    ti_type,
    id,
    cell_ids,
    milestone_ids,
    milestone_network,
    milestone_percentages,
    progressions,
    counts = counts,
    sample_info = sample_info,
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

#' An abstract data wrapper
#'
#' @param ti_type a descriptor specifying the TI type
#' @param id a unique identifier for the task
#' @param cell_ids the ids of the cells in the trajectory and counts
#' @param milestone_ids the ids of the milestones in the trajectory
#' @param milestone_network the network of the milestones
#' @param milestone_percentages what percentage of milestone is each cell
#' @param progressions what progression does a cell have
#' @param ... extra information to be stored in the wrapper
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
    progressions <- convert_milestone_percentages_to_progressions(cell_ids, milestone_ids, milestone_network, milestone_percentages)
  } else if (is.null(milestone_percentages)) {
    milestone_percentages <- convert_progressions_to_milestone_percentages(cell_ids, milestone_ids, milestone_network, progressions)
  }

  if (!is.data.frame(milestone_percentages) || ncol(milestone_percentages) != 3 || any(colnames(milestone_percentages) != c("cell_id", "milestone_id", "percentage"))) {
    stop(sQuote("milestone_percentages"), " should be a data frame with exactly four columns named ", sQuote("cell_id"),
         ", ", sQuote("milestone_id"), " and ", sQuote("percentage"), ".")
  }
  if (!is.data.frame(progressions) || ncol(progressions) != 4 || any(colnames(progressions) != c("cell_id", "from", "to", "percentage"))) {
    stop(sQuote("progressions"), " should be a data frame with exactly four columns named ", sQuote("cell_id"),
         ", ", sQuote("from"), ", ", sQuote("to"), " and ", sQuote("percentage"), ".")
  }

  ## create a separate state if some cells have been filtered out
  na_ids <- setdiff(cell_ids, unique(milestone_percentages$cell_id))
  if (length(na_ids) != 0) {
    new_mid <- "FILTERED_CELLS"
    milestone_percentages <- bind_rows(
      milestone_percentages,
      data_frame(cell_id = na_ids, milestone_id = new_mid, percentage = 1)
    )
    progressions <- bind_rows(
      progressions,
      data_frame(cell_id = na_ids, from = new_mid, to = new_mid, percentage = 1)
    )
    milestone_network <- dplyr::bind_rows(
      milestone_network,
      data_frame(from = milestone_ids, to = new_mid, length = max(milestone_network$length)*5)
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

