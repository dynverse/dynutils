#' Extend an object
#'
#' @param object A list
#' @param .class_name A class name to add
#' @param ... Extra information in the list
#'
#' @importFrom testthat expect_is expect_named expect_false
#'
#' @export
#'
#' @examples
#' library(purrr)
#' l <- list(important_number = 42) %>% add_class("my_list")
#' l %>% extend_with(
#'   .class_name = "improved_list",
#'   url = "https://github.com/dynverse/dynverse"
#' )
#' l
extend_with <- function(
  object,
  .class_name,
  ...
) {
  testthat::expect_is(object, "list")
  testthat::expect_is(.class_name, "character")

  extension <- list(...)

  if (is.null(names(extension)) && length(extension) == 1 && "list" %in% class(extension[[1]])) {
    extension <- extension[[1]]
  }

  testthat::expect_is(extension, "list")
  testthat::expect_named(extension)
  testthat::expect_false(any(names(extension) == ""))

  object[names(extension)] <- extension

  add_class(object, .class_name)
}
