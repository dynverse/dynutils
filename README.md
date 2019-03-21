
# dynutils

[![Build
Status](https://travis-ci.org/dynverse/dynutils.svg?branch=master)](https://travis-ci.org/dynverse/dynutils)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/dynverse/dynutils?branch=master&svg=true)](https://ci.appveyor.com/project/dynverse/dynutils)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/dynutils)](https://cran.r-project.org/package=dynutils)
[![Coverage
Status](https://codecov.io/gh/dynverse/dynutils/branch/master/graph/badge.svg)](https://codecov.io/gh/dynverse/dynutils?branch=master)

Provides common functionality for the dynverse packages. An overview of
this functionality is available using the `?dynutils` command or in the
[examples
vignette](https://CRAN.R-project.org/package=dynutils/vignettes/functionality.html).

dynverse is created to support the development, execution, and
benchmarking of trajectory inference methods. For more information,
check out [dynverse.org](dynverse.org).

## Latest changes

Check out `news(package = "dynutils")` or [NEWS.md](inst/NEWS.md) for a
full list of
changes.

<!-- This section gets automatically generated from inst/NEWS.md, and also generates inst/NEWS -->

### Recent changes in dynutils 1.0.2 (2019-03-21)

  - BUG FIX: Fix generation of error messages in `%all_in%`.

  - FUNCTIONALITY: Added `read_h5()` and `write_h5()` functions.

  - FUNCTIONALITY: Added `is_sparse()`.

  - FUNCTIONALITY: Added `update_news()` and `recent_news()`. So meta\!

  - FUNCTIONALITY: Moved `calculate_mean()` from dyneval to dynutils.

### Recent changes in dynutils 1.0.1 (2019-02-04)

  - DOCUMENTATION: Fixed functionality vignette not showing any code.

  - FUNCTIONALITY: Add `mapdf_lat()`. This function expects a data
    frame, which gets broken down into a list of lists before applying
    the FUN. It expects the FUN to return a list, which gets transformed
    into a tibble with `list_as_tibble()`.

  - FUNCTIONALITY: `extract_row_to_list()` now works with tidy
    evaluation.

  - TESTING: Expand tests for tibble helper fuctions `list_as_tibble()`
    and `extract_row_to_list()`.

  - FUNCTIONALITY: Add `safe_tempdir()` function. This function creates
    a unique temporary directory and creates it if necessary.

  - FUNCTIONALITY: Add helper assertion functions `%all_in%`,
    `%has_names%`, `is_bounded()` and `is_single_numeric()`.
