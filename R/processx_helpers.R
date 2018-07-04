#' Processx callback function for printing stdout or stderr
#'
#' @param x The stdout/stderr
#' @param proc The processx process
#'
#' @export
print_processx <- function(x, proc) {
  cat(x)
}
