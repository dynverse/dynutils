#' Attempts to convert a list of lists to a tibble
#'
#' @param list_of_rows The list to be converted to a tibble
#'
#' @return A tibble with the same number of rows as there were elements in \code{list_of_rows}
#' @export
#'
#' @importFrom testthat expect_equal
#' @importFrom stats setNames
#'
#' @examples
#' l <- list(list(a = 1, b = log10), list(a = 2, b = sqrt))
#' tib <- list_as_tibble(l)
#' tib
list_as_tibble <- function(list_of_rows) {
  object_classes <- list_of_rows %>% map(class)
  object_class <- object_classes[[1]]

  for (objcl in object_classes) {
    testthat::expect_equal(objcl, object_class)
  }

  list_names <- names(list_of_rows[[1]])

  tib <- lapply(seq_along(list_names), function(x) {
    colname <- list_names[[x]]
    list <- lapply(list_of_rows, function(z) z[[colname]])
    if (typeof(list[[1]]) != "list" && all(sapply(list, length) == 1)) {
      unlist(list, recursive = F)
    } else {
      list
    }
  }) %>% setNames(list_names) %>% as_tibble()

  attr(tib, ".object_class") <- object_class
  tib
}

#' Extracts one row from a tibble and converts it to a list
#'
#' @param tib the tibble
#' @param row_id the index of the row to be selected
#'
#' @return the corresponding row from the tibble as a list
#' @export
#'
#' @examples
#' l <- list(list(a = 1, b = log10), list(a = 2, b = sqrt))
#' tib <- list_as_tibble(l)
#'
#' extract_row_to_list(tib, 2)
extract_row_to_list <- function(tib, row_id) {
  object <- tib[row_id, ] %>% as.list %>% map(function(x) {
    if (is.null(x) | !is.list(x)) {
      x
    } else {
      x[[1]]
    }
  })

  if (!is.null(attr(tib, ".object_class"))) {
    class(object) <- attr(tib, ".object_class")
  }

  object
}
