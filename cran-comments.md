# dynutils 1.0.1

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

## Test environments
* local Fedora 28 installation, R 3.5.0
* OS X install (on travis-ci), R 3.5.0
* ubuntu 14.04 (on travis-ci), R 3.5.0
* win-builder (on appveyor), R 3.5.0

## R CMD check results
```
── R CMD check results ───────────────────────────────────── dynutils 1.0.1 ────
Duration: 38.2s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded
```

## Reverse dependencies

Revdepcheck is giving me the error message:
```
> revdepcheck::revdep_check(timeout = as.difftime(60, units = "mins"))
── INIT ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── Computing revdeps ──
── INSTALL ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 2 versions ──
Installing CRAN version of dynutils
also installing the dependencies ‘ini’, ‘curl’, ‘mime’, ‘openssl’, ‘prettyunits’, ‘xopen’, ‘clipr’, ‘clisymbols’, ‘fs’, ‘gh’, ‘whisker’, ‘backports’, ‘bindr’, ‘fansi’, ‘utf8’, ‘callr’, ‘cli’, ‘git2r’, ‘httr’, ‘jsonlite’, ‘memoise’, ‘pkgbuild’, ‘pkgload’, ‘rcmdcheck’, ‘remotes’, ‘rstudioapi’, ‘sessioninfo’, ‘usethis’, ‘withr’, ‘assertthat’, ‘R6’, ‘rprojroot’, ‘bindrcpp’, ‘pkgconfig’, ‘rlang’, ‘tidyselect’, ‘BH’, ‘plogr’, ‘ps’, ‘stringi’, ‘praise’, ‘pillar’, ‘crayon’, ‘devtools’, ‘desc’, ‘dplyr’, ‘glue’, ‘magrittr’, ‘processx’, ‘purrr’, ‘stringr’, ‘testthat’, ‘tibble’, ‘tidyr’

Installing DEV version of dynutils
These packages have more recent versions available.
Which would you like to update?

1:   digest (0.6.15 -> 0.6.18) [CRAN]

Enter one or more numbers separated by spaces, or an empty line to cancel
1: 
Installing 54 packages: assertthat, backports, BH, bindr, bindrcpp, callr, cli, clipr, clisymbols, crayon, curl, desc, devtools, dplyr, fansi, fs, gh, git2r, glue, httr, ini, jsonlite, magrittr, memoise, mime, openssl, pillar, pkgbuild, pkgconfig, pkgload, plogr, praise, prettyunits, processx, ps, purrr, R6, rcmdcheck, remotes, rlang, rprojroot, rstudioapi, sessioninfo, stringi, stringr, testthat, tibble, tidyr, tidyselect, usethis, utf8, whisker, withr, xopen
Error in utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  : 
  (converted from warning) installation of package ‘dplyr’ had non-zero exit status
```

However, I manually ran a R CMD check on SCORPIUS and got the following result:

```
── R CMD check results ───────────────────────────────────── SCORPIUS 1.0.3 ────
Duration: 2m 0.7s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded
```
