find_news <- function(package) {
  file <- system.file("NEWS.md", package = package)

  if (nchar(file) == 0) {
    stop(package, " does not have a NEWS.md!")
  }

  file
}



#' Update the news based on the md file
#'
#' @param package The package name
#' @param write Whether to overwrite news
#'
#' @export
update_news <- function(package, write = TRUE) {
  # Automatically update inst/NEWS
  news_md <- readr::read_lines(find_news(package))

  # creating NEWS for package
  news_normal <- news_md %>%
    str_replace_all("^# dynutils", "dynutils") %>%
    str_replace_all("\\[[^\\]]*\\]\\(([^\\)]*)\\)", "\\1")

  if (write) { readr::write_lines(news_normal, "inst/NEWS") }  else {news_normal}
}

# processes the news into tidy format
process_news <- function(package) {
  news_md <- readr::read_lines(find_news(package))

  ix <- which(stringr::str_detect(news_md, "^# "))
  matches <- stringr::str_match(news_md[ix], c("\\# ([A-Za-z0-9]*) ([0-9\\.]*) \\((.*)\\)"))
  version <- matches[, 3]
  release_data <- matches[, 4]
  items <- purrr::map2(ix, lead(ix, default = length(news_md)), function(start, end) {
    news_md[(start + 1):(end-1)]
  })
  text <- purrr::map2(ix, lead(ix, default = length(news_md)), function(start, end) {
    news_md[(start):(end-1)]
  })

  tibble(
    version = version,
    release_data = release_data,
    items = items,
    text = text
  )
}



#' Print the most recent news
#'
#' @param n Number of recent news to print
#' @inheritParams update_news
#'
#' @examples
#' recent_news("dynutils")
#'
#' @export
recent_news <- function(package, n = 2) {
  process_news(package) %>%
    slice(1:n) %>%
    pull(text) %>%
    unlist() %>%
    str_replace("^#", "### Recent changes in ") %>%
    paste0(collapse = "\n")
}
