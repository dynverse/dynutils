# dynutils 1.0.6

* MINOR CHANGE `calculate_distance()`: Allow for many more distance measures.

* BUG FIX: Import `desc::desc()` where needed.
   
## Test environments
* local Fedora installation, R 4.0
* ubuntu (on travis-ci), R 4.0
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
── INIT ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── Computing revdeps ──
── INSTALL ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 2 versions ──
Installing CRAN version of dynutils
also installing the dependencies ‘cli’, ‘utf8’, ‘R6’, ‘rprojroot’, ‘ellipsis’, ‘generics’, ‘glue’, ‘lifecycle’, ‘rlang’, ‘tidyselect’, ‘vctrs’, ‘pillar’, ‘RcppParallel’, ‘RcppArmadillo’, ‘stringi’, ‘fansi’, ‘pkgconfig’, ‘assertthat’, ‘crayon’, ‘desc’, ‘dplyr’, ‘magrittr’, ‘proxyC’, ‘purrr’, ‘Rcpp’, ‘remotes’, ‘stringr’, ‘tibble’

Installing DEV version of dynutils
Installing 28 packages: Rcpp, glue, vctrs, utf8, rlang, lifecycle, fansi, ellipsis, crayon, cli, magrittr, purrr, pkgconfig, pillar, stringi, RcppArmadillo, RcppParallel, tidyselect, tibble, R6, generics, rprojroot, stringr, remotes, proxyC, dplyr, desc, assertthat
── CHECK ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 8 packages ──
✓ babelwhale 1.0.1                       ── E: 1     | W: 0     | N: 0                                                                                                                                                                            
✓ lmds 0.1.0                             ── E: 0     | W: 0     | N: 1                                                                                                                                                                            
✓ dynparam 1.0.2                         ── E: 0     | W: 0     | N: 0                                                                                                                                                                            
✓ GillespieSSA2 0.2.8                    ── E: 0     | W: 0     | N: 1                                                                                                                                                                            
✓ dynwrap 1.2.2                          ── E: 1     | W: 0     | N: 0                                                                                                                                                                            
✓ SCORPIUS 1.0.7                         ── E: 0     | W: 0     | N: 0                                                                                                                                                                            
✓ dyngen 1.0.1                           ── E: 0     | W: 0     | N: 0                                                                                                                                                                            
✓ dyndimred 1.0.4                        ── E: 0     | W: 0     | N: 0                                                                                                                                                                            
OK: 8                                                                                                                                                                                                                                           
BROKEN: 0
Total time: 30 min
── REPORT ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Writing summary to 'revdep/README.md'
Writing problems to 'revdep/problems.md'
Writing failures to 'revdep/failures.md'
Writing CRAN report to 'revdep/cran.md'
```
