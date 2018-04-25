#' Check which packages are installed
#'
#' @param dependencies The names of the packages to be installed
#' @export
check_packages <- function(dependencies) {
  dependencies %in% rownames(installed.packages())
}

#' Installs the suggests of a particular package including information from the remotes
#'
#' @param package The package from which the remotes will be extracted
#'
#' @inheritParams check_packages
#' @export
install_packages <- function(dependencies, package=NULL) {
  dependencies <- dependencies[!check_packages(dependencies)]

  if(!is.null(package)) {
    remotes <- desc::desc_get_remotes(find.package("dynmethods")) %>%
      set_names(., stringr::str_replace(., ".*/([:alpha:]*).*", "\\1"))

    for (dependency in dependencies[dependencies %in% names(remotes)]) {
      devtools::install_github(remotes[[dependency]])
    }
  } else {
    remotes <- character()
  }

  for (dependency in dependencies[!dependencies %in% names(remotes)]) {
    devtools::install_cran(dependency)
  }

  message("Installed ", paste0(dependencies, collapse = ", "))
}
