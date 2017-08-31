#' Generate random string
#'
#' Generate a random string with first the current time, together with a random number
#' @export
random_time_string <- function() {
  paste0(gsub("-|\\s|:", "_", strptime(Sys.time(), "%Y-%m-%d %H:%M:%S")), "__", paste0(sample(seq_len(9), 10, replace=T), collapse=""))
}
