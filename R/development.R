#' Switching of development stage within the dynverse
#'
#' @param file The description file, defaults to DESCRIPTION
#' @param desc The read in description using the desc package
#'
#' @export
#' @importFrom stringr str_replace_all str_subset str_detect
switch_devel <- function(file = "DESCRIPTION", desc = desc::desc(file = file)) {
  # set version to 9000
  version <- as.character(desc$get_version())
  if (!endsWith(version, "9000")) {
    version <- gsub("^([0-9]+\\.[0-9]+\\.[0-9]+).*", "\\1.9000", version)
    desc$set_version(version)
  }

  # add dynverse remotes if needed, and set to devel
  dynverse_dependencies <-
    desc$get_remotes() %>%
    str_subset("dynverse/") %>%
    str_replace_all("dynverse/([A-Za-z0-9]*).*", "\\1")

  if (length(dynverse_dependencies) > 0) {
    needed_remotes <- paste0("dynverse/", dynverse_dependencies)
    current_remotes <- desc$get_remotes() %>% discard(str_detect, "dynverse/")
    new_remotes <- c(
      current_remotes,
      paste0(needed_remotes, "@devel")
    )
    if (length(new_remotes) > 0 || new_remotes[1] == "") {
      desc$set_remotes(new_remotes)
    }
  }

  desc$write(file = file)
}

#' @export
#' @rdname switch_devel
#' @importFrom stringr str_replace_all str_subset str_detect
switch_master <- function(file = "DESCRIPTION", desc = desc::desc(file = file)) {
  # set version to 9000
  version <- as.character(desc$get_version())
  if (endsWith(version, "9000")) {
    version <- gsub("^([0-9]+\\.[0-9]+\\.[0-9]+)\\..*", "\\1", version)
    desc$set_version(version)
  }

  # add dynverse remotes if needed, and set to master
  dynverse_dependencies <-
    desc$get_remotes() %>%
    str_subset("dynverse/") %>%
    str_replace_all("dynverse/([A-Za-z0-9]*).*", "\\1")

  if (length(dynverse_dependencies) > 0) {
    needed_remotes <- paste0("dynverse/", dynverse_dependencies)
    current_remotes <- desc$get_remotes() %>% discard(str_detect, "dynverse/")
    new_remotes <- c(
      current_remotes,
      paste0(needed_remotes, "@master")
    )
    if (length(new_remotes) > 0 || new_remotes[1] == "") {
      desc$set_remotes(new_remotes)
    }
  }

  desc$write(file = file)
}

#' @export
#' @rdname switch_devel
switch_cran <- function(file = "DESCRIPTION", desc = desc::desc(file = file)) {
  # version should already be ok
  version <- as.character(desc$get_version())
  assertthat::assert_that(grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", version))

  # remove remotes
  desc$del_remotes(".*")

  desc$write(file = file)
}
