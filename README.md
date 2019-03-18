
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

Check out `news(package = "dynwrap")` or [NEWS.md](inst/NEWS.md) for a
full list of
changes.

<!-- This section gets automatically generated from inst/NEWS.md, and also generates inst/NEWS -->

### Recent changes in dynwrap 1.0.0 (unreleased)

  - MAJOR CHANGE: Add support for Singularity 3.0, drop support for
    previous releases of Singularity and singularity-hub.

  - FEATURE: Add `create_ti_method_definition()` to create a definition
    from a local script.

  - DOCUMENTATION: Major update of all documentation for release of
    dynwrap v2

  - MINOR CHANGE: Rename `compute_tented_geodesic_distances()` to
    `compute_geodesic_distances()`

### Recent changes in dynwrap 0.3.1.2 (01-02-2019)

  - BUG FIX: `simplify_replace_edges()` would sometimes swap edges in
    milestone network around, but forget invert percentages.
  - BUG FIX: Close sinks when interupting the R process
  - MINOR CHANGE: Work with new babelwhale, which includes support for
    singularity 3.0
