#' Check which packages are installed
#'
#' @param dependencies The names of the packages to be installed
#'
#' @export
check_packages <- function(dependencies) {
  set_names(dependencies %in% rownames(installed.packages()), dependencies)
}

#' Installs the suggests of a particular package including information from the remotes
#'
#' @param package The package from which the remotes will be extracted
#'
#' @inheritParams check_packages
#'
#' @importFrom desc desc_get_remotes
#' @importFrom devtools install_github install_cran
#'
#' @export
install_packages <- function(dependencies, package = NULL) {
  dependencies <- dependencies[!check_packages(dependencies)]

  if (length(dependencies) > 0) {
    setRepositories(ind = 1:2) # set repositories to include bioconductor

    if(!is.null(package)) {
      remotes <- desc::desc_get_remotes(find.package(package)) %>%
        set_names(., stringr::str_replace(., ".*/([:alpha:]*).*", "\\1"))

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
