#' Format and interpolate a string
#'
#' Uses \code{\link[glue]{glue}}, but removes the class from the output.
#'
#' @param ... Parameters passed to \code{\link[glue]{glue}}.
#'
#' @importFrom glue glue
#'
#' @seealso \code{\link[glue]{glue}}
#'
#' @export
pritt <- function(...) {
  as.character(glue::glue(...))
}
