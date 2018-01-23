#' Run until error
#' Run a system command until the R parent gets an exit signal, or until the command completes
#'
#' @param command Character scalar, the command to run.
#' @export
run_until_exit <- function(command) {
  cmd <- processx::process$new(commandline=command, stdout = "|", stderr = "|")
  tryCatch(
    {
      cmd$poll_io(9999999) # wait for process to be finished
    },
    finally={
      cmd$kill()
      stop("Command was terminated", call.=FALSE)
    }
  )

  output <- cmd$read_all_output_lines()
  error <- cmd$read_all_error_lines()

  if (cmd$get_exit_status() != 0) {
    stop(glue::collapse(c("Error running command: ", crayon::white$bold(command), "-- output --", crayon::blue(output), "-- error --", crayon::red(error)), "\n"), call.=FALSE)
  }

  tibble::lst(
    output, error
  )
}
