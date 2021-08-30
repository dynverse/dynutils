# dynutils 1.0.7

 * MINOR CHANGE `calculate_distance()`: Convert matrices to CsparseMatrix instead of dgCMatrix.

 * MINOR CHANGE `calculate_distance()`: Pass through the `diag` argument to proxyC.
 
 * BUG FIX unit test: Order of attributes is not relevant.
   
## Test environments
* local Fedora installation, R 4.0
* ubuntu (on travis-ci), R 4.0
* win-builder (via devtools), R release
* win-builder (via devtools), R devel

## R CMD check results
```
── R CMD check results ───────────────────────────────────── dynutils 1.0.7 ────
Duration: 1m 19.5s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓

R CMD check succeeded
```

## Reverse dependencies

A reverse dependency check was run on all downstream dependencies.
(Summary at [revdep/README.md](revdep/README.md)). No new problems were found.

```
> revdepcheck::revdep_check(timeout = as.difftime(600, units = "mins"), num_workers = 30)
── CHECK ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 10 packages ──
✓ lmds 0.1.0                             ── E: 0     | W: 0     | N: 1
✓ babelwhale 1.0.3                       ── E: 1     | W: 0     | N: 0
✓ dynparam 1.0.2                         ── E: 0     | W: 0     | N: 0
✓ dynwrap 1.2.2                          ── E: 1     | W: 0     | N: 0
✓ GillespieSSA2 0.2.8                    ── E: 0     | W: 0     | N: 1
✓ SCORPIUS 1.0.8                         ── E: 0     | W: 0     | N: 0
✓ dynfeature 1.0.0                       ── E: 0     | W: 0     | N: 1
✓ dynplot 1.1.1                          ── E: 0     | W: 0     | N: 0
✓ dyndimred 1.0.4                        ── E: 0     | W: 0     | N: 0
✓ dyngen 1.0.2                           ── E: 0     | W: 0     | N: 0
OK: 10                                                                                                                                                                                          
BROKEN: 0
Total time: 33 min
```
