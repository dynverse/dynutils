#' Add a dimensionality reductio n to a data wrapper
#'
#' @param data_wrapper A data wrapper to extend upon.
#' @param dimred The dimensionality reduction matrix.
#' @param dimred_milestone An optional dimensionality reduction of the milestones.
#' @param ... extra information to be stored in the wrapper
#'
#' @export
#'
#' @importFrom testthat expect_equal expect_is expect_true
add_dimred_to_wrapper <- function(
  data_wrapper,
  dimred,
  dimred_milestone = NULL,
  ...
) {
  testthat::expect_is(data_wrapper, "dynutils::data_wrapper")

  cell_ids <- data_wrapper$cell_ids

  testthat::expect_is(dimred, "matrix")
  testthat::expect_equal(rownames(dimred), cell_ids)

  if (!is.null(dimred_milestone)) {
    testthat::expect_true(is_wrapper_with_trajectory(data_wrapper))

    milestone_ids <- data_wrapper$milestone_ids
    testthat::expect_is(dimred_milestone, "matrix")
    testthat::expect_equal(rownames(dimred_milestone), milestone_ids)
  }

  # create output structure
  out <- c(
    data_wrapper,
    list(
      dimred = dimred,
      dimred_milestone = dimred_milestone,
      ...
    ))
  class(out) <- c("dynutils::with_dimred", class(data_wrapper))
  out
}

#' Test whether an object is a data_wrapper and has dimred data
#'
#' @param object The object to be tested.
#'
#' @export
is_wrapper_with_dimred <- function(object) {
  is_data_wrapper(object) && "dynutils::with_dimred" %in% class(object)
}
