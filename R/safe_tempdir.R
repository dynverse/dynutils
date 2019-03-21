#' Create an empty temporary directory and return its path
#'
#' @param subfolder Name of a subfolder to be created
#'
#' @export
#'
#' @examples
#' \dontrun{
#' safe_tempdir("samson")
#' # "/tmp/Rtmp8xCGJe/file339a13bec763/samson"
#' }
safe_tempdir <- function(subfolder) {
  dir <- file.path(tempfile(), subfolder) %>%
    fix_macosx_tmp()

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  dir
}

fix_macosx_tmp <- function(path) {
  gsub("^/var/", "/tmp/", path)
}

