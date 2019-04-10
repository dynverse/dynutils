#' Format and interpolate a string
#'
#' Uses [glue], but removes the class from the output.
#'
#' @param ... `expressions`\cr Expressions string(s) to format, multiple inputs are concatenated together before formatting.
#' @param .sep `character(1): \sQuote{""}`\cr Separator used to separate elements.
#' @param .envir `environment: parent.frame()`\cr Environment to evaluate each expression in. Expressions are
#'   evaluated from left to right. If `.x` is an environment, the expressions are
#'   evaluated in that environment and `.envir` is ignored.
#' @param .open `character(1): \sQuote{\\\{}`\cr The opening delimiter. Doubling the
#'   full delimiter escapes it.
#' @param .close `character(1): \sQuote{\\\}}`\cr The closing delimiter. Doubling the
#'   full delimiter escapes it.
#'
#' @importFrom glue glue
#'
#' @seealso [glue]
#'
#' @export
#'
#' @examples
#' a <- 10
#' pritt("a: {a}")
pritt <- function(..., .sep = "", .envir = parent.frame(), .open = "{", .close = "}") {
  as.character(glue::glue(..., .sep = .sep, .envir = .envir, .open = .open, .close = .close))
}
