parse_remotes <- function(remotes) {
  str_replace(remotes, ".*/([:alpha:]*).*", "\\1") %>%
    set_names(remotes)
}

#' Install packages taking into account the remotes of another
#'
#' Useful for installing suggested packages with GitHub remotes.
#'
#' @param ... The names of the packages to be installed
#' @param package The package from which the remotes will be extracted. If used, this package does need to be installed.
#' @param prompt Whether to ask the user first for installation
#'
#' @importFrom desc desc_get_remotes
#' @importFrom devtools install_github install_cran
#' @importFrom utils setRepositories
#'
#' @export
#'
#' @examples
#' \dontrun{
#' install_packages("SCORPIUS", package = "dynmethods", prompt = TRUE)
#' }
install_packages <- function(..., package = NULL, prompt = FALSE) {
  dependencies <- unlist(list(...)) %>% discard(check_packages)

  if (length(dependencies) > 0) {
    if (prompt) {
      message(paste0(
        "Following packages have to be installed: ",
        glue::glue_collapse(crayon::bold(dependencies), ", ", last = " and ")
      ))
      answer <- ifelse (
        is.null(getOption("dynutils_testmodepromptresponse")),
        readline("Do you want to install these packages? (y/yes/1 or n/no/2): "),
        getOption("dynutils_testmodepromptresponse")
      )

      if (!answer %in% c("y", "yes", 1)) {
        stop("Installation was interrupted.")
      }
    }

    # set repositories to include bioconductor
    utils::setRepositories(ind = 1:4)

    message("Installing ", paste0(dependencies, collapse = ", "))

    # if a package is provided, check the remotes
    # to see whether to install the dependencies from given remotes
    if (!is.null(package)) {
      if (!check_packages(package)) {
        stop("Package ", sQuote(package), " needs to have been installed first!")
      }
      remotes <-
        find.package(package) %>%
        desc::desc_get_remotes() %>%
        parse_remotes()

      devtools::install_github(remotes[dependencies[dependencies %in% names(remotes)]])
    } else {
      remotes <- character()
    }

    # install other depencies from cran
    devtools::install_cran(dependencies[!dependencies %in% names(remotes)])

    # display message
    message("Installed ", paste0(dependencies, collapse = ", "))

    # return installed dependencies at the end
    dependencies
  } else {
    NULL
  }
}
