#' @importFrom stringr str_replace
parse_remotes <- function(remotes) {
  str_replace(remotes, ".*/([:alpha:]*).*", "\\1") %>%
    set_names(remotes)
}

#' Check package availability
#'
#' If the session is interactive, prompt the user whether to install the packages.
#'
#' @param ... The names of the packages to be checked
#' @param try_install Whether running interactivly, which will prompt the user before installation
#'
#' @importFrom remotes install_cran
#' @importFrom utils setRepositories
#'
#' @export
#'
#' @examples
#' \dontrun{
#' install_packages("SCORPIUS")
#' }
install_packages <- function(..., try_install = interactive()) {
  dependencies <- unlist(list(...)) %>% discard(check_packages)

  if (length(dependencies) > 0) {
    if (try_install) {
      message(paste0(
        "Following packages have to be installed: ",
        paste(crayon::bold(dependencies), collapse = ", "),
        "\n",
        "Do you want to install these packages? \n",
        "1: Yes [default]\n",
        "2: No"
      ))
      answer <- ifelse (
        is.null(getOption("dynutils_testmodepromptresponse")),
        readline(),
        getOption("dynutils_testmodepromptresponse")
      )

      if (answer %in% c("no", "n", "2")) {
        stop("Installation was interrupted.")
      }
    } else {
      stop(
        "Please install the following packages: \n",
        "  ", paste0("\"", dependencies, "\"", collapse = ",")
      )
    }

    # set repositories to include bioconductor
    utils::setRepositories(ind = 1:4)

    message("Installing ", paste0(dependencies, collapse = ", "))

    # install other depencies from cran
    remotes::install_cran(dependencies)

    # display message
    message("Installed ", paste0(dependencies, collapse = ", "))

    # return installed dependencies at the end
    dependencies
  } else {
    invisible(NULL)
  }
}
