#' Inherit default parameters from a list of super functions
#'
#' @param super_functions A list of super functions of which \code{fun} needs to inherit the default parameters
#' @param fun The function whose default parameters need to be overridden
#'
#' @return Function \code{fun}, but with the default parameters of the \code{super_functions}
#' @export
#'
#' @examples
#'
#' fun1 <- function(a = 10, b = 7) {
#'   runif(a, -b, b)
#' }
#'
#' fun2 <- function(c = 9) {
#'   2^c
#' }
#'
#' fun3 <- inherit_default_params(
#'   super = list(fun1, fun2),
#'   fun = function(a, b, c) {
#'     params <- as.list(environment())[formalArgs(fun3)]
#'     z <- fun1(a, b)
#'     y <- fun2(c)
#'     list(params = params, z = z, y = y)
#'   }
#' )
inherit_default_params <- function(super_functions, fun) {
  for (sup in super_functions) {
    argsup <- intersect(formalArgs(fun), formalArgs(sup))
    formals(fun)[argsup] <- formals(sup)[argsup]
  }
  fun
}



#' Update a list based on another list (overwrite existing items, adds not existing items).
#' Does not work recursively compared to modifyList
#'
#' @param x List to be updated
#' @param y List to update `x` with
#' @return Update list
#' @export
updateList <- function(x, y) {
  x[names(x) %in% names(y)] <- y[names(y) %in% names(x)]
  x <- c(x, y[!(names(y) %in% names(x))])
  x
}
