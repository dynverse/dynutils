# dynutils 1.0.5

 * BUG FIX `install_packages()`: will not try to install packages when the session is not interactive.
 
 * LICENSE: Change to MIT.
   
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
> revdepcheck::revdep_check(timeout = as.difftime(60, units = "mins"), num_workers = 8)
── INSTALL ─────────────────────────────────────────────────────── 2 versions ──
Installing CRAN version of dynutils
Installing DEV version of dynutils
Installing 30 packages: assertthat, crayon, desc, dplyr, magrittr, proxyC, purrr, Rcpp, remotes, stringr, tibble, backports, rlang, cli, fansi, pillar, pkgconfig, glue, utf8, vctrs, ellipsis, digest, R6, rprojroot, tidyselect, BH, plogr, RcppParallel, RcppArmadillo, stringi
── CHECK ───────────────────────────────────────────────────────── 6 packages ──
✔ babelwhale 1.0.1                       ── E: 1     | W: 0     | N: 0          
✔ lmds 0.1.0                             ── E: 0     | W: 0     | N: 1          
✔ GillespieSSA2 0.2.5                    ── E: 0     | W: 0     | N: 1          
✔ dynparam 1.0.0                         ── E: 0     | W: 0     | N: 1          
I SCORPIUS 1.0.5                         ── E: 1     | W: 0     | N: 0          
✔ dynwrap 1.1.4                          ── E: 1     | W: 0     | N: 0          
OK: 6                                                                         
BROKEN: 0
Total time: 27 min
── REPORT ──────────────────────────────────────────────────────────────────────
Writing summary to 'revdep/README.md'
Writing problems to 'revdep/problems.md'
Writing failures to 'revdep/failures.md'
```
