#' Run until error
#' Run a system command until the R parent gets an exit signal, or until the command completes
#'
#' @param commands Character scalar, the command to run.
#' @param bash Whether to run the command within a bash shell
#' @export
run_until_exit <- function(commands, bash = TRUE) {
  if (bash) {
    command <- paste0("/bin/bash -c ", "'", glue::collapse(commands, ";"), "'")
  } else {
    command <- glue::collapse(commands, "\n")
  }

  stdout <- tempfile()
  stderr <- tempfile()
  cmd <- processx::process$new(commandline=command, stdout = stdout, stderr = stderr)
  tryCatch(
    {
      cmd$wait()
    },
    finally={
      if (cmd$is_alive()) {
        cmd$kill()
      }
    }
  )

  output <- readLines(stdout)
  error <- readLines(stderr)

  if (cmd$get_exit_status() != 0) {
    print(cmd$get_exit_status())
    stop(
      glue::collapse(
        c(
          "Error running command: ",
          crayon::white$bold(command),
          "-- output --",
          crayon::blue(output),
          "-- error --",
          crayon::red(error)
        ),
        "\n"
      ),
      call. = FALSE
    )
  }

  tibble::lst(
    output,
    error
  )
}
