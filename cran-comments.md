# dynutils 1.0.6

* MINOR CHANGE `calculate_distance()`: Allow for many more distance measures.

* BUG FIX: Import `desc::desc()` where needed.
   
## Test environments
* local Fedora 31 installation, R 3.6.2
* ubuntu 16.04 (on travis-ci), R 3.6.0
* win-builder (via devtools), R release
* win-builder (via devtools), R devel

## R CMD check results
```
── R CMD check results ───────────────────────────────────── dynutils 1.0.5 ────
Duration: 1m 0.8s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

R CMD check succeeded
```

## Reverse dependencies

A reverse dependency check was run on all downstream dependencies.
(Summary at [revdep/README.md](revdep/README.md)). No new problems were found.

```
> revdepcheck::revdep_check(timeout = as.difftime(600, units = "mins"), num_workers = 30)
── INIT ────────────────────────────────────────────────────────────────────────
── INSTALL ─────────────────────────────────────────────────────────────────────
Installing CRAN version of dynutils
also installing the dependencies ‘cli’, ‘utf8’, ‘R6’, ‘rprojroot’, ‘ellipsis’, ‘generics’, ‘lifecycle’, ‘rlang’, ‘tidyselect’, ‘vctrs’, ‘RcppParallel’, ‘RcppArmadillo’, ‘fansi’, ‘pillar’, ‘pkgconfig’, ‘assertthat’, ‘crayon’, ‘desc’, ‘dplyr’, ‘proxyC’, ‘purrr’, ‘Rcpp’, ‘remotes’, ‘tibble’

Installing DEV version of dynutils
Installing 26 packages: digest, assertthat, vctrs, utf8, rlang, lifecycle, fansi, ellipsis, crayon, cli, Rcpp, magrittr, purrr, pkgconfig, pillar, RcppArmadillo, RcppParallel, tidyselect, tibble, R6, generics, rprojroot, remotes, proxyC, dplyr, desc
── CHECK ───────────────────────────────────────────────────────────────────────
✓ babelwhale 1.0.1                       ── E: 1     | W: 0     | N: 0                                                                                                                                                                   
✓ lmds 0.1.0                             ── E: 0     | W: 0     | N: 1                                                                                                                                                                   
✓ dynparam 1.0.2                         ── E: 0     | W: 0     | N: 0                                                                                                                                                                   
✓ dynwrap 1.2.1                          ── E: 1     | W: 0     | N: 2                                                                                                                                                                   
✓ GillespieSSA2 0.2.7                    ── E: 0     | W: 0     | N: 1                                                                                                                                                                   
✓ SCORPIUS 1.0.7                         ── E: 0     | W: 0     | N: 0                                                                                                                                                                   
OK: 6                                                                         
BROKEN: 0
Total time: 6 min
── REPORT ──────────────────────────────────────────────────────────────────────
Writing summary to 'revdep/README.md'
Writing problems to 'revdep/problems.md'
Writing failures to 'revdep/failures.md'
```
