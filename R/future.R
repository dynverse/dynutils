#' Wait for a handle to finish or kill it
#'
#' @param expr An R \code{\link[base]{expression}} to be evaluated.
#' @param wait_time How long to wait before killing it.
#' @param cancel_output_fun A function \code{function(time_elapsed){...}} to call when the total time
#'   has exceeded the \code{wait_time}.
#' @param check_interval The time waited between checking whether the future has already finished.
#' @param verbose Whether or not to print waiting times.
#' @param ... extra arguments to be passed to the \code{\link[future]{future}} call.
#'
#' @importFrom future future plan resolved value
#' @export
#'
#' @examples
#' dry_run <- wait_or_kill({1}, wait_time = 5, function(x) x, 1)
#' wait_or_kill(
#'   expr = {
#'     Sys.sleep(100) # really long function
#'     data.frame(result = "finished", time = sleeper_time)
#'   },
#'   wait_time = 10,
#'   cancel_output_fun = function(t) data.frame(result = "killed", time = t),
#'   check_interval = 1,
#'   verbose = TRUE
#' )
wait_or_kill <- function(expr, wait_time, cancel_output_fun, check_interval = 1, verbose = FALSE, ...) {
  expr <- substitute(expr)
  envir <- parent.frame()

  requireNamespace("future")
  start_time <- Sys.time()

  future_handle <- future(
    { expr },
    evaluator = plan("multisession"),
    substitute = FALSE,
    envir = envir,
    ...
  )

  time_waited <- 0
  while (!resolved(future_handle) && time_waited < wait_time) {
    Sys.sleep(check_interval)
    time_waited <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (verbose) cat("Time waited: ", time_waited, "\n", sep = "")
  }

  if (resolved(future_handle)) {
    value(future_handle)
  } else {
    future:::ClusterRegistry("stop")
    cancel_output_fun(time_waited)
  }
}
