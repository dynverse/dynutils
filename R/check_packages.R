#' Check which packages are installed
#'
#' @param ... A set of package names
#'
#' @export
#'
#' @importFrom utils installed.packages
#'
#' @examples
#' check_packages("SCORPIUS", "dynutils")
#' check_packages(c("princurve", "mlr", "tidyverse"))
check_packages <- function(...) {
  packages <- unlist(list(...))
  installed_packages <- map_lgl(packages, requireNamespace, quietly = TRUE)
  set_names(installed_packages, packages)
}
