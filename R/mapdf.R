#' @importFrom purrr as_mapper
mapdf_fun <- function(purrrfun) {
  function(.x, .f, ...) {
    .f <- purrr::as_mapper(.f, ...)
    purrrfun(seq_len(nrow(.x)), function(row_ix) {
      row <- extract_row_to_list(.x, row_ix)
      .f(row, ...)
    })
  }
}

#' Apply a function to each row of a data frame
#'
#' The mapdf functions transform their input by applying a function to each row of a data frame and returning a vector the same length as the input.
#' These functions work a lot like purrr's [map()] functions.
#'
#'  * `mapdf()` always returns a list.
#'
#'  * `mapdf_lgl()`, `mapdf_int()`, `mapdf_dbl()` and `mapdf_chr()` return vectors of the corresponding type (or die trying).
#'
#'  * `mapdf_dfr()` and `mapdf_dfc()` return data frames created by row-binding and column-binding respectively. They require dplyr to be installed.
#'
#'  * `mapdf_lat()` returns a tibble by transforming outputted lists to a tibble using [list_as_tibble].
#'
#'  * `walkdf()` calls .f for its side-effect and returns the input .x.
#'
#' @importFrom purrr map map_lgl map_chr map_int map_dbl map_dfr map_dfc walk map_if map_at
#'
#' @inheritParams purrr::map
#'
#' @param .x A data.frame, data_frame, or tibble.
#' @param .f A function or formula.
#'   If a function, the first argument will be the row as a list.
#'   If a formula, e.g. `~ .$a`, the `.` is a placeholder for the row as a list.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' tib <- tibble(
#'   a = c(1, 2),
#'   b = list(log10, sqrt),
#'   c = c("parrot", "quest"),
#'   .object_class = list(c("myobject", "list"), c("yourobject", "list"))
#' )
#'
#' # map over the rows using a function
#' tib %>% mapdf(class)
#'
#' # or use an anonymous function
#' tib %>% mapdf(function(row) paste0(row$b(row$a), "_", row$c))
#'
#' # or a formula
#' tib %>% mapdf(~ .$b)
#'
#' # there are many more variations available
#' # see ?mapdf for more info
#' tib %>% mapdf_lgl(~ .$a > 1)
#' tib %>% mapdf_chr(~ paste0("~", .$c, "~"))
#' tib %>% mapdf_int(~ nchar(.$c))
#' tib %>% mapdf_dbl(~ .$a * 1.234)
mapdf <- mapdf_fun(map)

#' @export
#' @rdname mapdf
mapdf_lgl <- mapdf_fun(map_lgl)

#' @export
#' @rdname mapdf
mapdf_chr <- mapdf_fun(map_chr)

#' @export
#' @rdname mapdf
mapdf_int <- mapdf_fun(map_int)

#' @export
#' @rdname mapdf
mapdf_dbl <- mapdf_fun(map_dbl)

#' @export
#' @rdname mapdf
mapdf_dfr <- mapdf_fun(map_dfr)

#' @export
#' @rdname mapdf
mapdf_dfc <- mapdf_fun(map_dfc)

#' @export
#' @rdname mapdf
mapdf_lat <- mapdf_fun(function(X, FUN) {
  list_as_tibble(map(X, FUN))
})

#' @export
#' @rdname mapdf
walkdf <- mapdf_fun(walk)

