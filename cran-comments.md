# dynutils 1.0.3 (2019-05-02)

 * FUNCTIONALITY: Added `test_h5_installation()`.
 
 * BUG FIX: Added compatibility workaround for being able to read hdf5 1.10.4 files
   on a hdf5 1.8.14 Windows platform (hhoeflin/hdf5r#60).
 
 * FUNCTIONALITY: `calculate_distance()` and `calculate_similarity()` now support
   sparse matrix calculation thanks to proxyC!
 
 * SMALL CHANGES: Remove dependencies: glue, Hmisc, devtools, testthat, tidyr.

## Test environments
* local Fedora 28 installation, R 3.5.0
* ubuntu 14.04 (on travis-ci), R 3.5.0
* win-builder (via devtools), R release
* win-builder (via devtools), R devel

## R CMD check results
```
── R CMD check results ───────────────────────────────────── dynutils 1.0.3 ────
Duration: 38.2s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded
```

## Reverse dependencies

* I ran a `revdep_check()` on all downstream dependencies.
  (Summary at [revdep/README.md](revdep/README.md)). 
  
```
> revdepcheck::revdep_check(timeout = as.difftime(60, units = "mins"))
── CHECK ───────────────────────────────────────────────────────────────────────────────────────────────────────────────── 1 packages ──
✔ SCORPIUS 1.0.2                         ── E: 0     | W: 0     | N: 0                                                                  
OK: 1                                                                                                                                 
BROKEN: 0
Total time: 7 min
── REPORT ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
```
