#' Disable setting seeds
#'
#' This funciton was adapted from \code{\link[utils]{assignInNamespace}}, but
#' modified to only allow overwriting \code{\link[base]{set.seed}}.
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
