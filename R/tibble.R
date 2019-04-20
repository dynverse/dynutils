#' Convert a list of lists to a tibble
#'
#' @param list_of_rows The list to be converted to a tibble
#'
#' @return A tibble with the same number of rows as there were elements in \code{list_of_rows}
#' @export
#'
#' @seealso tibble_as_list extract_row_to_list mapdf
#'
#' @examples
#' library(purrr)
#'
#' li <- list(
#'   list(a = 1, b = log10, c = "parrot") %>% add_class("myobject"),
#'   list(a = 2, b = sqrt, c = "quest") %>% add_class("yourobject")
#' )
#'
#' tib <- list_as_tibble(li)
#'
#' tib
list_as_tibble <- function(list_of_rows) {
  object_classes <- list_of_rows %>% map(class)

  list_names <- map(list_of_rows, names) %>% unlist() %>% unique()

  tib <- lapply(seq_along(list_names), function(x) {
    colname <- list_names[[x]]
    list <- lapply(list_of_rows, function(z) z[[colname]])
    list <- lapply(list, function(x) if(is.null(x)) {NA} else {x})
    if (typeof(list[[1]]) != "list" && all(sapply(list, length) == 1)) {
      unlist(list, recursive = F)
    } else {
      list
    }
  }) %>%
    set_names(list_names) %>%
    as_tibble()

  tib[[".object_class"]] <- object_classes
  tib
}

#' Extracts one row from a tibble and converts it to a list
#'
#' @param tib the tibble
#' @param row_id the index of the row to be selected, or alternatively an expression which will be evaluated to such an index
#'
#' @return the corresponding row from the tibble as a list
#' @export
#'
#' @seealso list_as_tibble tibble_as_list mapdf
#'
#' @examples
#' library(tibble)
#'
#' tib <- tibble(
#'   a = c(1, 2),
#'   b = list(log10, sqrt),
#'   c = c("parrot", "quest"),
#'   .object_class = list(c("myobject", "list"), c("yourobject", "list"))
#' )
#'
#' extract_row_to_list(tib, 2)
#' extract_row_to_list(tib, which(a == 1))
extract_row_to_list <- function(tib, row_id) {
  x <- enquo(row_id)
  if (!is.null(tib)) {
    object <- tib %>%
      slice(!!x)

    if (nrow(object) > 1) {
      stop("Multiple rows were matched")
    } else if (nrow(object) == 0) {
      stop("No rows were matched")
    }

    object <- object %>%
      as.list() %>%
      map(function(x) {
        if (is.null(x) | !is.list(x)) {
          x
        } else {
          x[[1]]
        }
      })

    if (".object_class" %in% colnames(tib)) {
      class(object) <- object[[".object_class"]]
      object[[".object_class"]] <- NULL
    }

    object
  } else {
    NULL
  }
}

#' Convert a tibble to a list of lists
#'
#' @param tib A tibble
#'
#' @return A list with the same number of lists as there were rows in \code{tib}
#' @export
#'
#' @seealso list_as_tibble extract_row_to_list mapdf
#'
#' @examples
#' library(tibble)
#'
#' tib <- tibble(
#'   a = c(1, 2),
#'   b = list(log10, sqrt),
#'   c = c("parrot", "quest"),
#'   .object_class = list(c("myobject", "list"), c("yourobject", "list"))
#' )
#'
#' li <- tibble_as_list(tib)
#'
#' li
tibble_as_list <- function(tib) {
  mapdf(tib, identity)
}
