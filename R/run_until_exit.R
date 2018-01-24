#' Run until error
#' Run a system command until the R parent gets an exit signal, or until the command completes
#'
#' @param command Character scalar, the command to run.
#' @export
run_until_exit <- function(commands, bash=TRUE) {
  if(bash) {
    command <- paste0("/bin/bash -c ", "'", glue::collapse(commands, ";"), "'")
  } else {
    command <- glue::collapse(commands, "\n")
  }

  cmd <- processx::process$new(commandline=command, stdout = "|", stderr = "|")
  tryCatch(
    {
      cmd$wait() # wait for process to be finished
    },
    finally={
      if (cmd$is_alive()) {
        cmd$kill()
      }
    }
  )

  output <- cmd$read_all_output_lines()
  error <- cmd$read_all_error_lines()

  if (cmd$get_exit_status() != 0) {
    print(cmd$get_exit_status())
    stop(glue::collapse(c("Error running command: ", crayon::white$bold(command), "-- output --", crayon::blue(output), "-- error --", crayon::red(error)), "\n"), call.=FALSE)
  }

  tibble::lst(
    output, error
  )
}
