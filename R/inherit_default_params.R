#' Inherit default parameters from a list of super functions
#'
#' @param super_functions A list of super functions of which \code{fun} needs to inherit the default parameters
#' @param fun The function whose default parameters need to be overridden
#'
#' @return Function \code{fun}, but with the default parameters of the \code{super_functions}
#' @export
#'
#' @examples
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
#'     x <- fun1(a, b)
#'     y <- fun2(c)
#'     list(x = x, y = y)
#'   }
#' )
#'
#' fun3(1, 2, 3)
#' fun3()
inherit_default_params <- function(super_functions, fun) {
  if (is.function(super_functions)) {
    super_functions <- list(super_functions)
  }
  for (sup in super_functions) {
    argsup <- intersect(formalArgs(fun), formalArgs(sup))
    formals(fun)[argsup] <- formals(sup)[argsup]
  }
  fun
}
