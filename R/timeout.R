#' Wait for a handle to finish or kill it
#'
#' @param expr An R \code{\link[base]{expression}} to be evaluated.
#' @param envir The environment in which the expression should be evaluated.
#' @param timeout How long to wait before killing it.
#' @param on_timeout A character specifying what action to take if a timeout event occurs. Must be one of: error, warning, silent.
#'
#' @importFrom parallel mcparallel mccollect
#' @importFrom tools pskill
#' @export
#'
#' @examples
#' \dontrun{
#' eval_with_timeout(
#'   expr = {
#'     Sys.sleep(100) # really long function
#'     data.frame(result = "finished", time = sleeper_time)
#'   },
#'   timeout = 10,
#' )
#' }
eval_with_timeout <- function(expr, envir = parent.frame(), timeout, on_timeout = c("error", "warning", "silent")) {
  # substitute expression so it is not executed as soon it is used
  expr <- substitute(expr)

  # match on_timeout
  on_timeout <- match.arg(on_timeout)

  # execute expr in separate fork
  myfork <- parallel::mcparallel({
    eval(expr, envir = envir)
  }, silent = FALSE)

  # wait max n seconds for a result.
  myresult <- parallel::mccollect(myfork, wait = FALSE, timeout = timeout)
  # kill fork after collect has returned
  tools::pskill(myfork$pid, tools::SIGKILL)
  tools::pskill(-1 * myfork$pid, tools::SIGKILL)

  # clean up:
  parallel::mccollect(myfork, wait = FALSE)

  # timeout?
  if (is.null(myresult)) {
    if (on_timeout == "error") {
      stop("reached elapsed time limit")
    } else if (on_timeout == "warning") {
      warning("reached elapsed time limit")
    } else if (on_timeout == "silent") {
      myresult <- NA
    }
  }

  # move this to distinguish between timeout and NULL returns
  myresult <- myresult[[1]]

  if ("try-error" %in% class(myresult)) {
    stop(attr(myresult, "condition"))
  }

  # send the buffered response
  return(myresult)
}

# old version
# wait_or_kill <- function(expr, wait_time, cancel_output_fun, check_interval = 1, verbose = FALSE, ...) {
#   expr <- substitute(expr)
#   envir <- parent.frame()
#
#   requireNamespace("future")
#   start_time <- Sys.time()
#
#   future_handle <- future(
#     { expr },
#     evaluator = plan("multisession"),
#     substitute = FALSE,
#     envir = envir,
#     ...
#   )
#
#   time_waited <- 0
#   while (!resolved(future_handle) && time_waited < wait_time) {
#     Sys.sleep(check_interval)
#     time_waited <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
#     if (verbose) cat("Time waited: ", time_waited, "\n", sep = "")
#   }
#
#   if (resolved(future_handle)) {
#     value(future_handle)
#   } else {
#     future:::ClusterRegistry("stop")
#     cancel_output_fun(time_waited)
#   }
# }
