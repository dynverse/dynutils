detect_package_name <- function() {
  if (!file.exists("DESCRIPTION")) {
    stop("Could not find DESCRIPTION file, please specify the package manually.")
  }

  lines <-
    readr::read_lines("DESCRIPTION") %>%
    keep(grepl("^Package: ", .)) %>%
    gsub("^Package: *", "", .)
}

find_news <- function(package) {
  file <- "inst/NEWS.md"

  if (!file.exists(file)) {
    file <- system.file("NEWS.md", package = package)
  }

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
update_news <- function(package = detect_package_name(), write = TRUE) {
  # Automatically update inst/NEWS
  news_md <- readr::read_lines(find_news(package))

  # creating NEWS for package
  news_normal <- news_md %>%
    str_replace_all(paste0("^# ", package), package) %>%
    str_replace_all("\\[[^\\]]*\\]\\(([^\\)]*)\\)", "\\1")

  if (write) {
    readr::write_lines(news_normal, "inst/NEWS")
  } else {
    news_normal
  }
}

# processes the news into tidy format
process_news <- function(package) {
  news_md <- readr::read_lines(find_news(package))

  ix <- which(stringr::str_detect(news_md, "^# "))
  matches <- stringr::str_match(news_md[ix], c("\\# ([A-Za-z0-9]*) ([0-9\\.]*) \\((.*)\\)"))
  version <- matches[, 3]
  release_data <- matches[, 4]
  items <- purrr::map2(ix, lead(ix - 1, default = length(news_md)), function(start, end) {
    news_md[(start + 1):(end)]
  })
  text <- purrr::map2(ix, lead(ix - 1, default = length(news_md)), function(start, end) {
    news_md[(start):(end)]
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
recent_news <- function(package = detect_package_name(), n = 2) {
  process_news(package) %>%
    slice(seq_len(n)) %>%
    pull(text) %>%
    unlist() %>%
    str_replace("^#", "### Recent changes in ") %>%
    paste0(collapse = "\n")
}
