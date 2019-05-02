detect_package_folder <- function(path = NULL) {
  path <-
    path %||% getOption("detecting_description_path") %||% "."

  paths <- lst(
    description = paste0(path, "/DESCRIPTION"),
    news_md = paste0(path, "/inst/NEWS.md"),
    news = paste0(path, "/inst/NEWS")
  )

  if (!file.exists(paths$description)) {
    stop("Could not find DESCRIPTION file, please specify the package manually.")
  }

  paths
}

detect_package_name <- function(path = NULL) {
  paths <- detect_package_folder(path = path)

  lines <- readr::read_lines(paths$description)
  lines <- lines[grepl("^Package: ", lines)]
  gsub("^Package: *", "", lines)
}

find_news <- function(path = NULL, package = detect_package_name(path = path)) {
  paths <- detect_package_folder(path = path)

  if (!file.exists(paths$news_md)) {
    stop(package, " does not have a NEWS.md!")
  }

  paths$news_md
}

#' Update the news based on the md file
#'
#' @param path The path of the description in which the package resides
#' @param package The package name
#' @param write Whether to overwrite news
#'
#' @export
update_news <- function(path = NULL, package = detect_package_name(path = path), write = TRUE) {
  paths <- detect_package_folder(path = path)

  # Automatically update inst/NEWS
  news_normal <-
    find_news(path = path, package = package) %>%
    readr::read_lines() %>%
    str_replace_all(paste0("^# ", package), package) %>%
    str_replace_all("\\[[^\\]]*\\]\\(([^\\)]*)\\)", "\\1")

  if (write) {
    readr::write_lines(news_normal, paths$news)
  } else {
    news_normal
  }
}

# processes the news into tidy format
#' @importFrom stringr str_detect str_match
#' @importFrom purrr map2
process_news <- function(path = NULL, package = detect_package_name(path = path)) {
  news_md <-
    find_news(path = path, package = package) %>%
    readr::read_lines()

  start_ix <- which(str_detect(news_md, "^# "))
  matches <- str_match(news_md[start_ix], c("\\# ([A-Za-z0-9]*) ([0-9\\.]*) \\((.*)\\)"))
  version <- matches[, 3]
  release_data <- matches[, 4]

  end_ix <- c(start_ix[-1] - 1, length(news_md))

  items <- map2(start_ix+1, end_ix, function(start, end) {
    news_md[start:end]
  })
  text <- map2(start_ix, end_ix, function(start, end) {
    news_md[start:end]
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
#' @export
#' @importFrom stringr str_replace
recent_news <- function(path = NULL, package = detect_package_name(path = path), n = 2) {
  news <-
    process_news(path = path, package = package) %>%
    slice(seq_len(n))

  news$text %>%
    unlist() %>%
    str_replace("^#", "### Recent changes in ") %>%
    paste0(collapse = "\n")
}
