#' Helper functions for generating timings in dynmethods
#'
#' @param timings_list The timings list of previous checkpoints
#' @param name The name of the timings checkpoint
#' @param object An object to attach the timings_list to, as a parameter
#'
#' @export
#'
#' @rdname timings_list
add_timing_checkpoint <- function(timings_list, name) {
  if (is.null(timings_list)) {
    timings_list <- list()
  }
  timings_list[[name]] <- Sys.time()
  timings_list
}

#' @rdname timings_list
#' @export
attach_timings_attribute <- function(object, timings_list) {
  attr(object, ".dynutils_timings_list") <- timings_list
  object
}

#' @rdname timings_list
#' @export
get_timings_attribute <- function(object) {
  attr(object, ".dynutils_timings_list")
}
