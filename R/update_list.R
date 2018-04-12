
#' Update a list based on another list (overwrite existing items, adds not existing items).
#' Does not work recursively.
#'
#' @param x List to be updated
#' @param y List to update \code{x} with
#' @return Update list
#' @export
merge_lists <- function(x, y) {
  x[names(y)] <- y
  x
}
