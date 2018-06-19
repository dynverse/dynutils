#' Run until error
#' Run a system command until the R parent gets an exit signal, or until the command completes
#'
#' @param commands Character scalar, the command to run.
#' @export
run_until_exit <- function(commands) {
  stdout <- tempfile()
  stderr <- tempfile()
  cmd <- processx::process$new(
    "bash",
    c("-c", commands),
    stdout = stdout,
    stderr = stderr
  )

  tryCatch(
    {
      cmd$wait()
    },
    finally = {
      if (cmd$is_alive()) {
        cmd$kill()
      }
    }
  )

  output <- readLines(stdout)
  error <- readLines(stderr)

  if (cmd$get_exit_status() != 0) {
    stop(
      glue::collapse(
        c(
          "Error status {cmd$get_exit_status()} while running command: ",
          crayon::white$bold(commands),
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
