#' Generate random string
#'
#' Generate a random string with first the current time, together with a random number
#'
#' @param name Optional string to be added in the random_time_string
#'
#' @export
#'
#' @examples
#' random_time_string("test")
random_time_string <- function(name = NULL) {
  paste0(
    format(Sys.time(), format = "%Y%m%d_%H%M%S"),
    "__",
    ifelse(!is.null(name), paste0(name, "__"), ""),
    paste(sample(c(LETTERS, letters, 0:9), 10, replace = T), collapse = "")
  )
}
