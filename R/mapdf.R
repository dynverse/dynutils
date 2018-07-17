mapdf_fun <- function(purrrfun) {
  function(.x, .f, ...) {
    purrrfun(seq_len(nrow(.x)), function(row_ix) {
      row <- extract_row_to_list(.x, row_ix)
      .f(row, ...)
    })
  }
}

#' Apply a function to each row of a data frame
#'
#' The mapdf functions transform their input by applying a function to each row of a data frame and returning a vector the same length as the input.
#' These functions work a lot like purrr's \code{\link[purrr]{map}} functions.
#'
#'  * \code{mapdf()}, \code{mapdf_if()} and \code{mapdf_at()} always return a list.
#'
#'  * \code{mapdf_lgl()}, \code{mapdf_int()}, \code{mapdf_dbl()} and \code{mapdf_chr()} return vectors of the corresponding type (or die trying).
#'
#'  * \code{mapdf_dfr()} and \code{mapdf_dfc()} return data frames created by row-binding and column-binding respectively. They require dplyr to be installed.
#'
#'  * \code{walkdf()} calls .f for its side-effect and returns the input .x.
#'
#' @importFrom purrr map map_lgl map_chr map_int map_dbl map_dfr map_dfc walk map_if map_at
#'
#' @inheritParams purrr::map
#'
#' @param .x A \code{\link[base]{data.frame}}, \code{\link[tibble]{data_frame}} or \code{\link[tibble]{tibble}}.
#' @param .f A function in the form of \code{function(l, ...) {}},
#'   where \code{l} will be one row from the data frame, converted as a list,
#'   and \code{...} can be any other arguments passed to the \code{mapdf} function.
#'
#' @export
#'
#' @examples
#' tib <- data.frame(x=1:3, y=3:5)
#' mapdf(tib, function(z) {z$x + z$y})
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
walkdf <- mapdf_fun(walk)

#' @export
#' @rdname mapdf
mapdf_if <- function(.x, .p, .f, ...) {
  map_if(seq_len(nrow(.x)), .p = .p, function(row_ix) {
    row <- extract_row_to_list(.x, row_ix)
    .f(row, ...)
  })
}

#' @export
#' @rdname mapdf
mapdf_at <- function(.x, .at, .f, ...) {
  map_at(seq_len(nrow(.x)), .at = .at, function(row_ix) {
    row <- extract_row_to_list(.x, row_ix)
    .f(row, ...)
  })
}
