#' Disable setting seeds
#'
#' This function was adapted from \code{assignInNamespace}, but
#' modified to only allow overwriting \code{set.seed}.
#'
#' @param value an R object.
#'
#' @export
override_setseed <- function (value) {
  ns <- asNamespace("base")
  x <- "set.seed"
  envir <- .BaseNamespaceEnv

  unlockBinding(x, ns)
  assign(x, value, envir = ns, inherits = FALSE)
  w <- options("warn")
  on.exit(options(w))
  options(warn = -1)
  lockBinding(x, ns)

  invisible(NULL)
}
