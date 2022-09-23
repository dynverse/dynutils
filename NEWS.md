# dynutils 1.0.9

 * BUG FIX `read_h5()` and `write_h5()`: Surround functionality with `if(requireNamespace(., quietly = TRUE))`.
 
 * Remove readr dependency.

# dynutils 1.0.8

 * BUG FIX unit test: Surround hdf5 unit tests with `if(require(.))`.

# dynutils 1.0.7

 * MINOR CHANGE `calculate_distance()`: Convert matrices to CsparseMatrix instead of dgCMatrix.

 * MINOR CHANGE `calculate_distance()`: Pass through the `diag` argument to proxyC.
 
 * BUG FIX unit test: Order of attributes is not relevant.

# dynutils 1.0.6

 * MINOR CHANGE `calculate_distance()`: Allow for many more distance measures.

 * BUG FIX: Import `desc::desc()` where needed.

# dynutils 1.0.5 (2020-02-21)

 * BUG FIX `install_packages()`: will not try to install packages when the session is not interactive.
 
 * LICENSE: Change to MIT.

# dynutils 1.0.4 (2019-10-03)

 * BUG FIX `recent_news()`: do not prepend subsections.
 
 * BUG FIX `switch_cran()`: add missing version variable.
 
 * REMOVAL `update_news()` remove this function as R 3.6 natively
   supports markdown news format.
 
# dynutils 1.0.3 (2019-05-02)

 * FUNCTIONALITY: Added `test_h5_installation()`.
 
 * BUG FIX: Added compatibility workaround for being able to read hdf5 1.10.4 files
   on a hdf5 1.8.14 Windows platform (hhoeflin/hdf5r#60).
 
 * FUNCTIONALITY: `calculate_distance()` and `calculate_similarity()` now support
   sparse matrix calculation thanks to proxyC!
 
 * SMALL CHANGES: Remove dependencies: glue, Hmisc, devtools, testthat, tidyr.

# dynutils 1.0.2 (2019-03-21)

 * BUG FIX: Fix generation of error messages in `%all_in%`.
 
 * FUNCTIONALITY: Added `read_h5()` and `write_h5()` functions.
 
 * FUNCTIONALITY: Added `is_sparse()`.
 
 * FUNCTIONALITY: Added `update_news()` and `recent_news()`. So meta!
 
 * FUNCTIONALITY: Moved `calculate_mean()` from dyneval to dynutils.
 
# dynutils 1.0.1 (2019-02-04) 

 * DOCUMENTATION: Fixed functionality vignette not showing any code.

 * FUNCTIONALITY: Add `mapdf_lat()`. This function expects a data frame,
   which gets broken down into a list of lists before applying the FUN.
   It expects the FUN to return a list, which gets transformed into a tibble
   with `list_as_tibble()`.

 * FUNCTIONALITY: `extract_row_to_list()` now works with tidy evaluation.

 * TESTING: Expand tests for tibble helper fuctions `list_as_tibble()` and
   `extract_row_to_list()`.

 * FUNCTIONALITY: Add `safe_tempdir()` function. This function creates a
   unique temporary directory and creates it if necessary.
   
 * FUNCTIONALITY: Add helper assertion functions 
   `%all_in%`, `%has_names%`, `is_bounded()` and `is_single_numeric()`.
 
# dynutils 1.0.0 (2018-07-18)
 
 * REMOVED `run_until_exit()`: This functionality is already 
   sufficiently covered by the `processx` package.
   
 * ADDED `mapdf()` functions: Apply a function to each row of a data frame.
   These functions are very similar to the `purrr::map()` functions.
 
 * ADDED `tibble_as_list()`: Reverse functionality of `list_as_tibble()`.
 
 * ADDED `project_to_segments()`: Project a set of points to to set of segments.
 
 * DOCUMENTATION: Added documentation to every exported function.
 
 * DOCUMENTATION: Added overview of functions to `?dynutils`.
 
 * DOCUMENTATION: Added overview of functionality to `README.md`.
 
 * TESTING: Expanded tests for tibble helper functions,
   `calculate_distance()`, and `install_packages()`.
   
 * DOCUMENTATION: Added `inst/NEWS.md` file.
 
# dynutils 0.1.0 (2018-06-24)

 * INITIAL RELEASE ON CRAN: 
   dynutils provides common functionality for the dynverse packages.
