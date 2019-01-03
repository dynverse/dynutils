#' Check whether a value is a single numeric
#'
#' @param x A value to be tested
#'
#' @export
is_single_numeric <- function(x) {
  length(x) == 1 && is.numeric(x) && !is.na(x)
}
on_failure(is_single_numeric) <- function(call, env) {
  paste0(deparse(call$x), " is not a single numeric value")
}

#' Check whether a value within a certain interval
#'
#' @param x A value to be tested
#' @param lower_bound The lower bound
#' @param lower_closed Whether the lower bound is closed
#' @param upper_bound The upper bound
#' @param upper_closed Whether the upper bound is closed
#'
#' @export
is_bounded <- function(x, lower_bound = -Inf, lower_closed = FALSE, upper_bound = Inf, upper_closed = FALSE) {
  all(
    is.numeric(x) & !is.na(x) &
    {if (lower_closed) lower_bound <= x else lower_bound < x} &
    {if (upper_closed) x <= upper_bound else x < upper_bound}
  )
}
on_failure(is_bounded) <- function(call, env) {
  left_sym <- if (eval(call$lower_closed) %||% formals(is_bounded)$lower_closed) "[" else "("
  right_sym <- if (eval(call$upper_closed) %||% formals(is_bounded)$upper_closed) "]" else ")"
  left_bound <- eval(call$lower_bound) %||% formals(is_bounded)$lower_bound
  right_bound <- eval(call$upper_bound) %||% formals(is_bounded)$upper_bound
  paste0(deparse(call$x), " is not bounded by ", left_sym, left_bound, ",", right_bound, right_sym)
}

#' Check whether an object has certain names
#'
#' @param x object to test
#' @param which name
#'
#' @export
#' @importFrom assertthat %has_name%
has_names <- function(x, which) {
  all(x %has_name% which)
}
on_failure(has_names) <- function(call, env) {
  paste0(deparse(call$x), " does not have names ", deparse(setdiff(eval(call$which, env), names(eval(call$x, env)))))
}
#' @rdname has_names
#' @export
`%has_names%` <- has_names

#' Check whether a vector are all elements of another vector
#'
#' @param x The values to be matched.
#' @param table The values to be matched against.
#'
#' @export
all_in <- function(x, table) {
  all(x %in% table)
}
on_failure(all_in) <- function(call, env) {
  x <- eval(call$x, env)
  table <- eval(call$table, env)
  elements <- deparse(setdiff(table, x)) %>% paste(collapse = "")
  paste0(deparse(call$x), " is missing elements ", elements)
}

#' @rdname all_in
#' @export
`%allin%` <- all_in
