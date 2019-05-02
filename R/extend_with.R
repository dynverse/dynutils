#' Extend an object
#'
#' @param object A list
#' @param .class_name A class name to add
#' @param ... Extra information in the list
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
  assert_that(
    is.list(object),
    is.character(.class_name)
  )

  extension <- list(...)

  if (is.null(names(extension)) && length(extension) == 1 && "list" %in% class(extension[[1]])) {
    extension <- extension[[1]]
  }

  assert_that(
    is.list(extension),
    !is.null(names(extension)),
    all(names(extension) != "")
  )

  object[names(extension)] <- extension

  add_class(object, .class_name)
}
