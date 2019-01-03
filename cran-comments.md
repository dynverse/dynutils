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
   `%allin%`, `%has_names%`, `is_bounded()` and `is_single_numeric()`.

## Test environments
* local Fedora 28 installation, R 3.5.0
* OS X install (on travis-ci), R 3.5.0
* ubuntu 14.04 (on travis-ci), R 3.5.0
* win-builder (on appveyor), R 3.5.0

## R CMD check results
```
── R CMD check results ──────────────────────────────── dynutils 1.0.0.9000 ────
Duration: 38.2s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded
```

## Reverse dependencies

TO DO:
```
revdepcheck::revdep_reset()
revdepcheck::revdep_check(num_workers = 8, timeout = as.difftime(60, units = "mins"))
```
