
#' Update a list based on another list (overwrite existing items, adds not existing items).
#' Does not work recursively.
#'
#' @param x List to be updated
#' @param y List to update `x` with
#' @return Update list
#' @export
update_list <- function(x, y) {
  x[names(x) %in% names(y)] <- y[names(y) %in% names(x)]
  x <- c(x, y[!(names(y) %in% names(x))])
  x
}
