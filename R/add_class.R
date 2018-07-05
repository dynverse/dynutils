#' Add class to object whilst keeping the old classes
#'
#' @inheritParams base::class
#' @param class A character vector naming classes
#'
#' @export
#'
#' @examples
#' library(purrr)
#' l <- list(important_number = 42) %>% add_class("my_list")
add_class <- function(x, class) {
  class(x) <- c(class, class(x))
  x
}
