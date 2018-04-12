#' Format and interpolate a string
#'
#' Uses \code{\link[glue]{glue}}, but removes the class from the output.
#'
#' @inheritParams glue::glue
#'
#' @importFrom glue glue
#'
#' @seealso \code{\link[glue]{glue}}
#'
#' @export
pritt <- function(..., .sep = "", .envir = parent.frame(), .open = "{", .close = "}") {
  as.character(glue::glue(..., .sep = .sep, .envir = .envir, .open = .open, .close = .close))
}

#' Format, interpolate and print a string
#'
#' Uses \code{\link{pritt}} and \code{\link[base]{cat}}
#'
#' @param ... \[`expressions`]\cr Expressions string(s) to format, multiple inputs are concatenated together before formatting.
#'
#' @export
sticky_cat <- function(...) {
  cat(pritt(...))
}
