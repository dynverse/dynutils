#' Inherit default parameters from a list of super functions
#'
#' @param super_functions A list of super functions of which `fun`` needs to inherit the default parameters
#' @param fun The function whose default parameters need to be overridden
#'
#' @return Function `fun`, but with the default parameters of the `super_functions`
#' @export
#'
#' @importFrom methods formalArgs
#'
#' @examples
#' fun1 <- function(a = 10, b = 7) runif(a, -b, b)
#' fun2 <- function(c = 9) 2^c
#'
#' fun3 <- inherit_default_params(
#'   super = list(fun1, fun2),
#'   fun = function(a, b, c) {
#'     list(x = fun1(a, b), y = fun2(c))
#'   }
#' )
#'
#' fun3
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
