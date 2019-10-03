#' Common functionality for the dynverse packages
#'
#' Provides common functionality for the dynverse packages.
#' dynverse is created to support the development, execution, and benchmarking of trajectory inference methods.
#' For more information, check out [dynverse.org](https://dynverse.org).
#'
#' @section Manipulation of lists:
#'   * [add_class()]: Add a class to an object
#'   * [extend_with()]: Extend list with more data
#'
#' @section Calculations:
#'   * [calculate_distance()]: Calculate pairwise distances between two (sparse) matrices
#'   * [calculate_similarity()]: Calculate pairwise similarities between two (sparse) matrices
#'   * [calculate_mean()]: Calculate a (weighted) mean between vectors or a list of vectors; supports the arithmetic, geometric and harmonic mean
#'   * [project_to_segments()]: Project a set of points to to set of segments
#'
#' @section Manipulation of matrices:
#'   * [expand_matrix()]: Add rows and columns to a matrix
#'
#' @section Scaling of matrices and vectors:
#'   * [scale_uniform()]: Rescale data to have a certain center and max range
#'   * [scale_minmax()]: Rescale data to a \[0, 1\] range
#'   * [scale_quantile()]: Cut off outer quantiles and rescale to a \[0, 1\] range
#'
#' @section Manipulation of functions:
#'   * [inherit_default_params()]: Have one function inherit the default parameters from other functions
#'
#' @section Manipulation of packages:
#'   * [check_packages()]: Easily checking whether certain packages are installed
#'   * [install_packages()]: Install packages taking into account the remotes of another
#'
#' @section Manipulation of vectors:
#'   * [random_time_string()]: Generates a string very likely to be unique
#'
#' @section Tibble helpers:
#'   * [list_as_tibble()]: Convert a list of lists to a tibble whilst retaining class information
#'   * [tibble_as_list()]: Convert a tibble back to a list of lists whilst retaining class information
#'   * [extract_row_to_list()]: Extracts one row from a tibble and converts it to a list
#'   * [mapdf()]: Apply a function to each row of a data frame
#'
#' @section File helpers:
#'   * [safe_tempdir()]: Create an empty temporary directory and return its path
#'
#' @section Assertion helpers:
#'   * [%all_in%()]: Check whether a vector are all elements of another vector
#'   * [%has_names%()]: Check whether an object has certain names
#'   * [is_single_numeric()]: Check whether a value is a single numeric
#'   * [is_bounded()]: Check whether a value within a certain interval
#'
#' @section Package helpers:
#'   * [recent_news()]: Print the most recent news (assumes NEWS.md file as specified by [news()])
#'
#' @importFrom dplyr slice enquo n
#' @importFrom tibble tibble as_tibble lst
#' @importFrom purrr %>% %||% map map_df map_chr map_lgl map_int map_dbl keep discard set_names
#' @importFrom magrittr %<>% %$% set_rownames set_colnames
#' @importFrom assertthat assert_that on_failure<-
#'
#' @useDynLib dynutils
#' @importFrom Rcpp evalCpp
#'
#' @docType package
#' @name dynutils
NULL
