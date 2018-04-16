#' Add class to object whilst keeping the old classes
#'
#' @inheritParams base::class
#' @param class A character vector naming classes
#'
#' @export
add_class <- function(x, class) {
  class(x) <- c(class, class(x))
  x
}
