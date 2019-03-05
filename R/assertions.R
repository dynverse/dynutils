#' Check whether a value is a single numeric
#'
#' @param x A value to be tested
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(assertthat)
#' assert_that(is_single_numeric(1))
#' # TRUE
#'
#' assert_that(is_single_numeric(Inf))
#' # TRUE
#'
#' assert_that(is_single_numeric(1.6))
#' # TRUE
#'
#' assert_that(is_single_numeric(NA))
#' # Error: NA is not a single numeric value
#'
#' assert_that(is_single_numeric(1:6))
#' # Error: 1:6 is not a single numeric value
#'
#' assert_that(is_single_numeric("pie"))
#' # Error: "pie" is not a single numeric value
#' }
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
#'
#' @examples
#' \dontrun{
#' library(assertthat)
#' assert_that(is_bounded(10))
#' # TRUE
#'
#' assert_that(is_bounded(10:30))
#' # TRUE
#'
#' assert_that(is_bounded(Inf))
#' # Error: Inf is not bounded by (-Inf,Inf)
#'
#' assert_that(is_bounded(10, lower_bound = 20))
#' # Error: 10 is not bounded by (20,Inf)
#'
#' assert_that(is_bounded(
#'   10,
#'   lower_bound = 20,
#'   lower_closed = TRUE,
#'   upper_bound = 30,
#'   upper_closed = FALSE
#' ))
#' # Error: 10 is not bounded by [20,30)
#' }
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
  left_bound <- eval(call$lower_bound) %||% eval(formals(is_bounded)$lower_bound)
  right_bound <- eval(call$upper_bound) %||% eval(formals(is_bounded)$upper_bound)
  paste0(deparse(call$x), " is not bounded by ", left_sym, left_bound, ",", right_bound, right_sym)
}

#' Check whether an object has certain names
#'
#' @param x object to test
#' @param which name
#'
#' @export
#' @importFrom assertthat %has_name%
#'
#' @examples
#' \dontrun{
#' library(assertthat)
#' li <- list(a = 1, b = 2)
#'
#' assert_that(li %has_names% "a")
#' # TRUE
#'
#' assert_that(li %has_names% "c")
#' # Error: li is missing 1 name from "c": "c"
#'
#' assert_that(li %has_names% letters)
#' # Error: li is missing 24 names from letters: "c", "d", "e", ...
#' }
has_names <- function(x, which) {
  all(x %has_name% which)
}
on_failure(has_names) <- function(call, env) {
  paste0(deparse(call$x), " does not have names ", deparse(setdiff(eval(call$which, env), names(eval(call$x, env)))))
}
on_failure(has_names) <- function(call, env) {
  x <- names(eval(call$x, env))
  which <- eval(call$which, env)
  elements <- setdiff(which, x)
  elem_str <-
    elements %>%
    head(3) %>%
    map_chr(deparse) %>%
    paste(collapse = ", ")

  paste0(
    deparse(call$x),
    " is missing ",
    length(elements),
    " name",
    ifelse(length(elements) == 1, "", "s"),
    " from ",
    deparse(call$which),
    ": ",
    elem_str,
    ifelse(length(elements) > 3, ", ...", "")
  )
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
#'
#' @examples
#' \dontrun{
#' library(assertthat)
#' assert_that(c(1, 2) %all_in% c(0, 1, 2, 3, 4))
#' # TRUE
#'
#' assert_that("a" %all_in% letters)
#' # TRUE
#'
#' assert_that("A" %all_in% letters)
#' # Error: "A" is missing 1 element from letters: "A"
#'
#' assert_that(1:10 %all_in% letters)
#' # Error: 1:10 is missing 10 elements from letters: 1L, 2L, 3L, ...
#' }
all_in <- function(x, table) {
  all(x %in% table)
}
on_failure(all_in) <- function(call, env) {
  x <- eval(call$x, env)
  table <- eval(call$table, env)
  elements <- setdiff(x, table)
  elem_str <-
    elements %>%
    head(3) %>%
    map_chr(deparse, nlines = 1) %>%
    paste(collapse = ", ")

  paste0(
    paste0(deparse(call$x, nlines = 3), collapse= " "),
    " is missing ",
    length(elements),
    " element",
    ifelse(length(elements) == 1, "", "s"),
    " from ",
    paste0(deparse(call$table, nlines = 3), collapse= " "),
    ": ",
    elem_str,
    ifelse(length(elements) > 3, ", ...", "")
  )
}

#' @rdname all_in
#' @export
`%all_in%` <- all_in
