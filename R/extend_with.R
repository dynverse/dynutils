#' Extend an object
#'
#' @param object A list
#' @param class_name A class name to add
#' @param extension Extra information in the list
#'
#' @export
extend_with <- function(
  object,
  class_name,
  extension
) {
  testthat::expect_is(object, "list")
  testthat::expect_is(class_name, "character")
  testthat::expect_is(extension, "list")

  object[names(extension)] <- extension

  add_class(object, class_name)
}
