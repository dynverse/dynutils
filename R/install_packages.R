#' Check which packages are installed
#'
#' @param dependencies The names of the packages to be installed
#'
#' @export
#'
#' @importFrom utils installed.packages
check_packages <- function(dependencies) {
  set_names(dependencies %in% rownames(utils::installed.packages()), dependencies)
}

parse_remotes <- function(remotes) {
  str_replace(remotes, ".*/([:alpha:]*).*", "\\1") %>%
    set_names(remotes)
}

#' Installs the suggests of a particular package including information from the remotes
#'
#' @param package The package from which the remotes will be extracted
#' @param prompt Whether to ask the user first for installation
#'
#' @inheritParams check_packages
#'
#' @importFrom desc desc_get_remotes
#' @importFrom devtools install_github install_cran
#' @importFrom utils setRepositories
#'
#' @export
install_packages <- function(dependencies, package = NULL, prompt = FALSE) {
  dependencies <- dependencies[!check_packages(dependencies)]

  if (length(dependencies) > 0) {
    if (prompt) {
      message(paste0(
        "Following packages have to be installed: ",
        glue::collapse(crayon::bold(dependencies), ", ", last = " and ")
      ))
      answer <- readline("Do you want to install them: y/yes/1 or n/no/2? ")

      if (answer %in% c("n", "no", 2)) {
        stop("Cannot run method without required packages.")
      }
    }

    utils::setRepositories(ind = 1:4) # set repositories to include bioconductor

    message("Installing ", paste0(dependencies, collapse = ", "))

    if(!is.null(package)) {
      remotes <- desc::desc_get_remotes(find.package(package)) %>%
        parse_remotes()

      devtools::install_github(remotes[dependencies[dependencies %in% names(remotes)]])
    } else {
      remotes <- character()
    }

    devtools::install_cran(dependencies[!dependencies %in% names(remotes)])

    # display message
    message("Installed ", paste0(dependencies, collapse = ", "))

    # return installed dependencies at the end
    dependencies
  } else {
    NULL
  }
}
