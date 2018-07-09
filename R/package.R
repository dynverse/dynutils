#' Common functionality for the dynverse packages
#'
#' Provides common functionality for the dynverse packages.
#' dynverse is created to support the development, execution, and benchmarking of trajectory inference methods.
#' For more information, check out \url{https://github.com/dynverse/dynverse}.
#'
#' @section Manipulation of lists:
#' \itemize{
#'   \item{\code{\link{add_class}}: Add a class to an object}
#'   \item{\code{\link{extend_with}}: Extend list with more data}
#' }
#'
#' @section Distances:
#' \itemize{
#'   \item{\code{\link{calculate_distance}}: Compute pairwise distances between two matrices}
#' }
#'
#' @section Manipulation of matrices:
#' \itemize{
#'   \item{\code{\link{expand_matrix}}: Add rows and columns to a matrix}
#' }
#'
#' @section Scaling of matrices and vectors:
#' \itemize{
#'   \item{\code{\link{scale_uniform}}: Rescale data to have a certain center and max range}
#'   \item{\code{\link{scale_minmax}}: Rescale data to a [0, 1] range}
#'   \item{\code{\link{scale_quantile}}: Cut off outer quantiles and rescale to a [0, 1] range}
#' }
#'
#' @section Manipulation of functions:
#' \itemize{
#'   \item{\code{\link{inherit_default_params}}: Have one function inherit the default parameters from other functions}
#' }
#'
#' @section Manipulation of packages:
#' \itemize{
#'   \item{\code{\link{check_packages}}: Easily checking whether certain packages are installed}
#'   \item{\code{\link{install_packages}}: Install packages taking into account the remotes of another}
#' }
#'
#' @section Manipulation of character vectors:
#' \itemize{
#'   \item{\code{\link{pritt}}: A friendly version of \code{\link[glue:glue]{glue::glue}}}
#'   \item{\code{\link{random_time_string}}: Generates a string very likely to be unique}
#' }
#'
#'
#'
#' @import dplyr
#' @import tidyr
#' @import methods
#' @import tibble
#' @import stringr
#' @importFrom purrr %>% map map_df map_chr map_lgl map_int map_dbl keep discard set_names
#' @importFrom magrittr %<>% %$% set_rownames set_colnames
#'
#' @useDynLib dynutils
#'
#' @docType package
#' @name dynutils
NULL
