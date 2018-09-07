#' Get temporary directory
#'
#' @param subfolder Name of a subfolder to be created
#'
#' @export
safe_tempdir <- function(subfolder) {
  dir <- file.path(tempfile(), subfolder) %>%
    fix_macosx_tmp()

  if (dir.exists(dir)) {
    unlink(dir, recursive = TRUE, force = TRUE)
  }

  dir.create(dir, recursive = TRUE)

  dir
}

fix_macosx_tmp <- function(path) {
  gsub("^/var/", "/tmp/", path)
}

